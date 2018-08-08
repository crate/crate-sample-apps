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
%%% http handler for the /post/:id/like endpoint, increasing likes for a post
%%% @end
%%%-------------------------------------------------------------------
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
-include("guestbook_cors.hrl").

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {craterl, ClientRef} = lists:keyfind(craterl, 1, Opts),
  Req2 = addCORSHeaders(Req),
  {ok, Req2, #{craterl => ClientRef}}.

allowed_methods(Req, State) ->
  {[<<"PUT">>, <<"OPTIONS">>], Req, State}.

resource_exists(Req, #{craterl := CrateClientRef}=State) ->
  {Method, Req3} = cowboy_req:method(Req),
  resource_exists(Method, Req3, State, CrateClientRef).
resource_exists(<<"OPTIONS">>, Req, State, CrateClientRef) ->
  %% collection of posts always exists
  CrateClientRef,
  {true, Req, State};
resource_exists(<<"PUT">>, Req, State, CrateClientRef) ->
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
