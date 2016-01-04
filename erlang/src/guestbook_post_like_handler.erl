-module(guestbook_post_like_handler).


%% API
-export([
  init/3,
  rest_init/2,
  allowed_methods/2,
  resource_exists/2,
  content_types_accepted/2,
  like_post/2
]).
-include("guestbook.hrl").

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {craterl, ClientRef} = lists:keyfind(craterl, 1, Opts),
  {ok, Req, #{craterl => ClientRef}}.

allowed_methods(Req, State) ->
  {[<<"PUT">>], Req, State}.

resource_exists(Req, #{craterl := CrateClientRef}=State) ->
  {Id, Req2} = cowboy_req:binding(id, Req),
  %% do the actual update to the model here, so we save 1 GET
  case guestbook_post_model:like_post(CrateClientRef, Id) of
    {ok, Post} ->
      {true, Req2, State#{post => Post}};
    {error, not_found} ->
      Req3 = guestbook_handlers:error_message(404,  "Post with id=\"" ++ binary_to_list(Id) ++ "\" not found", Req2),
      {false, Req3, State};
    {error, Message} ->
      handle_error(500, Message, Req2, State)
  end.

content_types_accepted(Req, State) ->
  {[
    {'*', like_post}
  ], Req, State}.

like_post(Req, #{post := LikedPost} = State) ->
  %% actual like performed in resource_exists
  EncodedPost = jsx:encode(LikedPost),
  Req2 = cowboy_req:set_resp_body(EncodedPost, Req),
  {true, Req2, State};
like_post(Req, State) ->
  {ok, Req2} = cowboy_req:reply(404, [?JSON_CT_HEADER], Req),
  {halt, Req2, State}.

handle_error(StatusCode, Message, Req, State) ->
  Req2 = guestbook_handlers:error_response(StatusCode, Message, Req),
  {halt, Req2, State}.