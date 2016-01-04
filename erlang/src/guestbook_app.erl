%%%-------------------------------------------------------------------
%% @doc guestbook public API
%% @end
%%%-------------------------------------------------------------------

-module('guestbook_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1
        ,error_response/4]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  ListenPort = application:get_env(guestbook, listen_port, 8080),
  CraterlServerSpecs = lists:map(
      fun craterl_url:server_spec/1, application:get_env(guestbook, crate_hosts, ["localhost:4200"])),
  CraterlClient = craterl:new(CraterlServerSpecs),
  ok = prepare_models(CraterlClient),
  HandlerOpts = [{craterl, CraterlClient}],
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/posts/", guestbook_posts_handler, HandlerOpts},
      {"/post/:id", guestbook_post_handler, HandlerOpts},
      {"/post/:id/like", guestbook_post_like_handler, HandlerOpts},
      {"/search", guestbook_search_handler, HandlerOpts},
      {"/images", guestbook_images_handler, HandlerOpts},
      {"/image/:digest", guestbook_image_handler, HandlerOpts}
    ]}
  ]),
  {ok, _HttpPid} = cowboy:start_http(guestbook_listener, 100,
    [{port, ListenPort}],
    [
      {env, [{dispatch, Dispatch}]},
      {onresponse, fun ?MODULE:error_response/4}]),
  {ok, SupPid} = 'guestbook_sup':start_link(),
  {ok, SupPid, CraterlClient}.

%%--------------------------------------------------------------------
stop(CraterlClient) ->
  craterl:stop_client(CraterlClient),
  cowboy:stop_listener(guestbook_listener),
  ok.

prepare_models(CraterlClient) ->
  ok = guestbook_image_model:prepare_images(CraterlClient),
  ok = guestbook_country_model:prepare_countries(CraterlClient),
  ok = guestbook_post_model:prepare_posts(CraterlClient).

error_response(Status, Headers, <<>>, Req) when Status >= 400 ->
  Message = if
    Status < 500 -> <<"invalid request">>;
    true -> <<"something went wrong">>
  end,
  Body = jsx:encode(#{
    error => Message,
    code => Status
  }),
  Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
    {<<"content-length">>, integer_to_list(byte_size(Body))}),
  {ok, Req2} = cowboy_req:reply(Status, Headers2, Body, Req),
  Req2;
error_response(_Status, _Headers, _Body, Req) -> Req.