%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, CRATE Technology GmbH
%%% Licensed to CRATE Technology GmbH ("Crate") under one or more contributor
%%% license agreements.  See the NOTICE file distributed with this work for
%%% additional information regarding copyright ownership.  Crate licenses
%%% this file to you under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.  You may
%%% obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
%%% License for the specific language governing permissions and limitations
%%% under the License.
%%%
%%% However, if you have executed another commercial license agreement
%%% with Crate these terms will supersede the license and you may use the
%%% software solely pursuant to the terms of the relevant commercial agreement.
%%% @doc
%%%
%%% This is the model part that abstracts the inner workings of the
%%% storing and retrieving of the post entities to a database, Crate in this case.
%%%
%%% this module returns all its posts as maps, formatted by row_to_map/2 below
%%%
%%% @end
%%%-------------------------------------------------------------------
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

-type post() :: map().
-export_type([post/0]).

%%% SQL STATEMENTS
-define(SCHEMA, <<"CREATE TABLE IF NOT EXISTS guestbook.posts (
    id STRING PRIMARY KEY,
    \"user\" OBJECT(STRICT) AS (
      name STRING,
      location GEO_POINT
    ),
    text STRING INDEX USING FULLTEXT WITH (analyzer = 'english'),
    created TIMESTAMP,
    image_ref STRING,
    like_count LONG
  ) WITH (number_of_replicas = '0-2')">>).
-define(COMMON_SELECT, <<"SELECT posts.id as id,
                              posts.\"user\" as \"user\",
                              posts.text as text,
                              posts.created as created,
                              posts.image_ref as image_ref,
                              posts.like_count as like_count,
                              countries.geometry as area,
                              countries.name as country
                            FROM guestbook.posts as posts INNER JOIN guestbook.countries as countries
                            ON within(posts.\"user\"['location'], countries.geometry)">>).  %% TODO: we need outer joins
-define(GET_SINGLE_STMT, <<?COMMON_SELECT/binary, " WHERE posts.id = ? LIMIT 1">>).
-define(GET_STMT, <<?COMMON_SELECT/binary, " ORDER BY posts.created DESC">>).
-define(SEARCH_STMT, <<?COMMON_SELECT/binary, " WHERE match(text, ?)
                                                ORDER BY posts._score DESC, posts.created DESC">>).
-define(DELETE_STMT, <<"DELETE FROM guestbook.posts WHERE id = ?">>).
-define(INSERT_STMT, <<"INSERT INTO guestbook.posts (id, image_ref, text, \"user\", created, like_count)
                                             VALUES (?,  ?,         ?,    ?,    CURRENT_TIMESTAMP, 0)">>).
-define(REFRESH_STMT, <<"REFRESH TABLE guestbook.posts">>).
-define(UPDATE_STMT, <<"UPDATE guestbook.posts SET text=? WHERE id=?">>).
-define(LIKE_STMT, <<"UPDATE guestbook.posts SET like_count = like_count + 1 WHERE id=?">>).

%% setup the posts table if it does not exist yet
%% idempotent
-spec prepare_posts(craterl:craterl_client_ref()) -> ok.
prepare_posts(CraterlClient) ->
  {ok, _Response} = craterl:sql(CraterlClient, ?SCHEMA),
  ok.

%% get a single post by id (as list of 1 element or not_found atom)
-spec get_post(CraterlClientRef::craterl:craterl_client_ref(), Id::binary()) -> [post()]|not_found.
get_post(CraterlClientRef, Id) ->
  {ok, Response} = craterl:sql(CraterlClientRef, ?GET_SINGLE_STMT, [Id]),
  case craterl_resp:row_count(Response) of
    1 ->
      [Row] = craterl_resp:rows(Response),
      row_to_map(craterl_resp:column_names(Response), Row);
    _ -> not_found
  end.

%% get all posts as list
-spec get_posts(CraterlClientRef::craterl:craterl_client_ref()) -> [post()].
get_posts(CraterlClientRef) ->
  {ok, Response} = craterl:sql(CraterlClientRef, ?GET_STMT),
  rows_to_maps(Response).

%% get all posts whose text matches SearchTerm as list
-spec search_posts(CraterlClientRef::craterl:craterl_client_ref(), SearchTerm::binary()) -> [post()]|not_found.
search_posts(CraterlClientRef, SearchTerm) ->
  {ok, Response} = craterl:sql(CraterlClientRef, ?SEARCH_STMT, [SearchTerm]),
  rows_to_maps(Response).

%% create a post from a json payload
%%
%% creates a random UUIDv4 id for every created post
%%
%% we need to refresh the table after creation in order to make it available
%% for a consecutive fetch of the inserted post
%% as we need to return it here.
-spec create_post(CraterlClientRef::craterl:craterl_client_ref(), Payload::map()) -> {ok, post()}|{error, term()}.
create_post(CraterlClientRef, #{<<"text">> := Text,
                                <<"user">> := User}=Payload) when is_map(User) and is_binary(Text) ->
  Id = uuid:uuid_to_string(uuid:get_v4(strong), binary_standard),
  ImageRef = maps:get(<<"image_ref">>, Payload, null),
  case craterl:sql(CraterlClientRef, ?INSERT_STMT, [Id, ImageRef, Text, User]) of
    {ok, _Response} ->
      refresh_posts(CraterlClientRef),
      Post = get_post(CraterlClientRef, Id),
      {ok, Post};
    {error, Error} -> {error, craterl_resp:error_message(Error)}
  end.

%% delete a post by id
-spec delete_post(CraterlClientRef::craterl:craterl_client_ref(), Id::binary()) -> ok|not_found.
delete_post(CraterlClientRef, Id) ->
  {ok, Response} = craterl:sql(CraterlClientRef, ?DELETE_STMT, [Id]),
  case craterl_resp:row_count(Response) of
    1 ->
      %% we do a refresh here, just in case
      refresh_posts(CraterlClientRef),
      ok;
    0 -> not_found
  end.

%% update a post by id and set its text and return the updated post
-spec update_post(CraterlClientRef::craterl:craterl_client_ref(), Id::binary(), Payload::map()) -> {ok, post()}|{error, term()}.
update_post(CraterlClientRef, Id, #{<<"text">> := Text}) ->
  case craterl:sql(CraterlClientRef, ?UPDATE_STMT, [Text, Id]) of
    {ok, Response} ->
      case craterl_resp:row_count(Response) of
        1 ->
          %% we need to refresh here again
          refresh_posts(CraterlClientRef),
          Post = get_post(CraterlClientRef, Id),
          {ok, Post};
        0 ->
          {error, not_found}
      end;
    {error, Error} ->
      {error, craterl_resp:error_message(Error)}
  end.

%% increment the like_count of a post and return the updated post
-spec like_post(CraterlClientRef::craterl:craterl_client_ref(), Id::binary()) -> {ok, post()}|{error, term()}.
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

-spec rows_to_maps(craterl:sql_response()) -> [post()].
rows_to_maps(Response) ->
  ColumnNames = craterl_resp:column_names(Response),
  MapFun = fun (Row) -> row_to_map(ColumnNames, Row) end,
  lists:map(MapFun, craterl_resp:rows(Response)).

-spec row_to_map([binary()], [binary()]) -> [post()].
row_to_map(Names, Row) ->
  maps:from_list(lists:zip(Names ,Row)).

%% refresh posts in order to make the latest changes available for queries
-spec refresh_posts(CraterlClientRef::craterl:craterl_client_ref()) -> ok.
refresh_posts(CraterlClientRef) ->
  {ok, _RefreshResponse} = craterl:sql(CraterlClientRef, ?REFRESH_STMT),
  ok.
