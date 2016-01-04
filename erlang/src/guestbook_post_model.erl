-module(guestbook_post_model).

%%% API
-export([
  prepare_posts/1,
  get_post/2,
  get_posts/1,
  create_post/2,
  delete_post/2,
  update_post/3,
  search_posts/2,
  like_post/2
  ]).

%%% STATEMENTS
-define(SCHEMA, <<"CREATE TABLE IF NOT EXISTS guestbook.posts (
    id STRING PRIMARY KEY,
    user OBJECT(STRICT) AS (
      name STRING,
      location GEO_POINT
    ),
    text STRING INDEX USING FULLTEXT WITH (analyzer = 'english'),
    created TIMESTAMP,
    image_ref STRING,
    like_count LONG
  ) WITH (number_of_replicas = '0-2')">>).
-define(COMMON_SELECT, <<"SELECT posts.id as id,
                              posts.user as user,
                              posts.text as text,
                              posts.created as created,
                              posts.image_ref as image_ref,
                              posts.like_count as like_count,
                              countries.geometry as area,
                              countries.name as country
                            FROM guestbook.posts as posts INNER JOIN guestbook.countries as countries
                            ON within(posts.user['location'], countries.geometry)">>).  %% TODO: we need outer joins
-define(GET_SINGLE_STMT, <<?COMMON_SELECT/binary, " WHERE posts.id = ? LIMIT 1">>).
-define(GET_STMT, <<?COMMON_SELECT/binary, " ORDER BY posts.created DESC">>).
-define(SEARCH_STMT, <<?COMMON_SELECT/binary, " WHERE match(text, ?)
                                                ORDER BY posts._score DESC, posts.created DESC">>).
-define(DELETE_STMT, <<"DELETE FROM guestbook.posts WHERE id = ?">>).
-define(INSERT_STMT, <<"INSERT INTO guestbook.posts (id, image_ref, text, user, created, like_count)
                                             VALUES (?,  ?,         ?,    ?,    CURRENT_TIMESTAMP, 0)">>).
-define(REFRESH_STMT, <<"REFRESH TABLE guestbook.posts">>).
-define(UPDATE_STMT, <<"UPDATE guestbook.posts SET text=? WHERE id=?">>).
-define(LIKE_STMT, <<"UPDATE guestbook.posts SET like_count = like_count + 1 WHERE id=?">>).

prepare_posts(CraterlClient) ->
  {ok, _Response} = craterl:sql(CraterlClient, ?SCHEMA),
  ok.

get_post(CraterlClientRef, Id) ->
  {ok, Response} = craterl:sql(CraterlClientRef, ?GET_SINGLE_STMT, [Id]),
  case craterl_resp:row_count(Response) of
    1 ->
      [Row] = craterl_resp:rows(Response),
      row_to_map(craterl_resp:column_names(Response), Row);
    _ -> not_found
  end.

get_posts(CraterlClientRef) ->
  {ok, Response} = craterl:sql(CraterlClientRef, ?GET_STMT),
  rows_to_maps(Response).

search_posts(CraterlClientRef, SearchTerm) ->
  {ok, Response} = craterl:sql(CraterlClientRef, ?SEARCH_STMT, [SearchTerm]),
  rows_to_maps(Response).

create_post(CraterlClientRef, #{<<"text">> := Text,
                                <<"user">> := User}=Payload) when is_map(User) and is_binary(Text) ->
  Id = uuid:uuid_to_string(uuid:get_v4(weak), binary_standard),
  ImageRef = maps:get(<<"image_ref">>, Payload, null),
  case craterl:sql(CraterlClientRef, ?INSERT_STMT, [Id, ImageRef, Text, User]) of
    {ok, _Response} ->
      refresh_posts(CraterlClientRef),
      Post = get_post(CraterlClientRef, Id),
      {ok, Post};
    {error, Error} -> {error, craterl_resp:error_message(Error)}
  end.

delete_post(CraterlClientRef, Id) ->
  {ok, Response} = craterl:sql(CraterlClientRef, ?DELETE_STMT, [Id]),
  case craterl_resp:row_count(Response) of
    1 ->
      refresh_posts(CraterlClientRef),
      ok;
    0 -> not_found
  end.

update_post(CraterlClientRef, Id, #{<<"text">> := Text}) ->
  case craterl:sql(CraterlClientRef, ?UPDATE_STMT, [Text, Id]) of
    {ok, Response} ->
      case craterl_resp:row_count(Response) of
        1 ->
          refresh_posts(CraterlClientRef),
          Post = get_post(CraterlClientRef, Id),
          {ok, Post};
        0 ->
          {error, not_found}
      end;
    {error, Error} ->
      {error, craterl_resp:error_message(Error)}
  end.

like_post(CraterlClientRef, Id) ->
  case craterl:sql(CraterlClientRef, ?LIKE_STMT, [Id]) of
    {ok, Response} ->
      case craterl_resp:row_count(Response) of
        1 ->
          refresh_posts(CraterlClientRef),
          Post = get_post(CraterlClientRef, Id),
          {ok, Post};
        0 ->
          {error, not_found}
      end;
    {error, Error} ->
      {error, craterl_resp:error_message(Error)}
  end.

%%% PRIVATE %%%

rows_to_maps(Response) ->
  ColumnNames = craterl_resp:column_names(Response),
  MapFun = fun (Row) -> row_to_map(ColumnNames, Row) end,
  lists:map(MapFun, craterl_resp:rows(Response)).

row_to_map(Names, Row) ->
  maps:from_list(lists:zip(Names ,Row)).

refresh_posts(CraterlClientRef) ->
  {ok, _RefreshResponse} = craterl:sql(CraterlClientRef, ?REFRESH_STMT).