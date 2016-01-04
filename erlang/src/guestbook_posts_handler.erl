-module(guestbook_posts_handler).

%% API
-export([
  init/3,
  rest_init/2,
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2,
  malformed_request/2,
  resource_exists/2
]).

-export([
  post_from_json/2,
  post_to_json/2
]).

-include("guestbook.hrl").
init({_Transport, _Type}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {craterl, ClientRef} = lists:keyfind(craterl, 1, Opts),
  {ok, Req, #{craterl => ClientRef}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) -> {
  [
    {?JSON_CONTENT_TYPE, post_from_json}
  ],
  Req, State
}.

content_types_provided(Req, State) ->
  {[
    {?JSON_CONTENT_TYPE, post_to_json}
  ], Req, State}.

malformed_request(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  malformed_request(Method, Req2, State).
malformed_request(<<"POST">>, Req, State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  case Body of
    <<>> ->
      Req3 = guestbook_handlers:error_message(400, <<"Body is required">>, Req2),
      {true, Req3, State};
    _ ->
      try
        case jsx:decode(Body, [return_maps]) of
          %% verify correct body with pattern matching
          #{<<"text">> := _Text,
            <<"user">> := #{
              <<"location">> := [_Lat, _Lon],
              <<"name">> := _Name
            }} = Decoded ->
            TooManyKeys = maps:size(Decoded) > 3,
            {TooManyKeys, Req2, State#{payload => Decoded}};
          Invalid ->
            io:format("invalid payload: ~p~n", [Invalid]),
            Req3 = guestbook_handlers:error_message(400, "invalid json", Req2),
            {true, Req3, State}
        end
      catch
        error:badarg ->
          Req5 = guestbook_handlers:error_message(400, "invalid json", Req2),
          {true, Req5, State}
      end
  end;
malformed_request(<<"GET">>, Req, State) ->
  {false, Req, State}.

resource_exists(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  resource_exists(Method, Req2, State).
resource_exists(<<"GET">>, Req, State) ->
  %% collection of posts always exists
  {true, Req, State};
resource_exists(<<"POST">>, Req, State) ->
  %% POST requests always create new posts
  {false, Req, State}.

post_from_json(Req, #{craterl := CraterlClientRef, payload := Payload}=State) ->
  {ok, Post} = guestbook_post_model:create_post(CraterlClientRef, Payload),
  EncodedPost = jsx:encode([Post]),
  % return custom 201 although 200 would be appropriate according to cowboy
  {ok, Req2} = cowboy_req:reply(201, [?JSON_CT_HEADER], EncodedPost, Req),
  {halt, Req2, State}.

post_to_json(Req, #{craterl := CraterlClientRef}=State) ->
  Posts = guestbook_post_model:get_posts(CraterlClientRef),
  {jsx:encode(Posts), Req, State}.