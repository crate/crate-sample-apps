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
%%%
%%% @doc
%%% This is the entry point of the erlang applications,
%%% started by the OTP application manager.
%%% here we configure the Crate client and the webserver.
%%% every erlang application has a supervisor,
%%% but we don't need to supervise anything as all our
%%% code will be supervised by the cowboy and craterl applications.
%%%
%%% for some more info about this app, take a look at
%%%     guestbook_posts_handler.erl
%%% and guestbook_post_model.erl
%%%
%%% these modules are well documented so you get a glimpse of how this app is structured
%%% @end
%%%-------------------------------------------------------------------


-module('guestbook_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1
        ,error_response/4]).

start(_StartType, _StartArgs) ->
  ListenPort = application:get_env(guestbook, listen_port, 8080),

  %% the craterl client hosts to connect to are configured in
  %% guestbook.app.src
  CraterlServerSpecs = lists:map(
      fun craterl_url:server_spec/1, application:get_env(guestbook, crate_hosts, ["localhost:4200"])),
  CraterlClient = craterl:new(CraterlServerSpecs),

  %% set up the correct tables etc.
  ok = prepare_models(CraterlClient),
  HandlerOpts = [{craterl, CraterlClient}],

  %% these are the HTTP routes to support
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
      {onresponse, fun ?MODULE:error_response/4}]), % a custom error response hook, see below
  {ok, SupPid} = 'guestbook_sup':start_link(),
  {ok, SupPid, CraterlClient}.

%%--------------------------------------------------------------------
stop(CraterlClient) ->
  craterl:stop_client(CraterlClient),
  cowboy:stop_listener(guestbook_listener),
  ok.

%%
%% here we setup the database in a mostly idempotent way,
%% which means e.g. if table already exists, it will not be touched
%%
prepare_models(CraterlClient) ->
  ok = guestbook_image_model:prepare_images(CraterlClient),
  ok = guestbook_country_model:prepare_countries(CraterlClient),
  ok = guestbook_post_model:prepare_posts(CraterlClient).

%%
%% this is a response hook, that injects a
%% JSON response body with some error description
%% if none is set yet
%%
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
