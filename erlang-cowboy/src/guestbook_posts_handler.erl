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
%%% This is the HTTP handler for the /posts endpoint
%%% it is built as a cowboy_rest_handler which is documented
%%%      here: http://ninenines.eu/docs/en/cowboy/1.0/guide/rest_handlers/
%%%  and here: http://ninenines.eu/docs/en/cowboy/1.0/guide/rest_flowcharts/
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(guestbook_posts_handler).

%% these are all the cowboy_rest_handler callbacks
%% we needed to "override"
%% they all take a Request and a State as their arguments
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
  posts_to_json/2
]).

-include("guestbook.hrl").
-include("guestbook_cors.hrl").

init({_Transport, _Type}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {craterl, ClientRef} = lists:keyfind(craterl, 1, Opts),
  Req2 = addCORSHeaders(Req),
  {ok, Req2, #{craterl => ClientRef}}. %% we use a map as state

%% returns the allowed methods of this handler
%% GET and POST
allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

%% configures a separate callback for each accepted content-type
content_types_accepted(Req, State) -> {
  [
    {'*', post_from_json}
  ],
  Req, State
}.

%% configures callback handlers for producing a certain content type
%% from stuff in the State
content_types_provided(Req, State) ->
  {[
    {?JSON_CONTENT_TYPE, posts_to_json}
  ], Req, State}.

%% checks if the request is valid
%% dispatches by Request method
%% as get requests are always well-formed e.g.
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
malformed_request(<<"OPTIONS">>, Req, State) ->
  {false, Req, State};
malformed_request(<<"GET">>, Req, State) ->
  {false, Req, State}.

%% check if the resource exists
resource_exists(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  resource_exists(Method, Req2, State).
resource_exists(<<"OPTIONS">>, Req, State) ->
  %% collection of posts always exists
  {true, Req, State};
resource_exists(<<"GET">>, Req, State) ->
  %% collection of posts always exists
  {true, Req, State};
resource_exists(<<"POST">>, Req, State) ->
  %% POST requests always create new posts
  {false, Req, State}.

%% create a post from a json payload
%% this is the callback for application/json POST requests,
%% configured in content_types_accepted
%%
%% the payload in the state has been set by malformed_request/2 above
%% as we had to fetch the payload in order to verify its validity
%% and we don't want to fetch it again here
post_from_json(Req, #{craterl := CraterlClientRef, payload := Payload}=State) ->
  {ok, Post} = guestbook_post_model:create_post(CraterlClientRef, Payload),
  EncodedPost = jsx:encode([Post]),
  % return custom 201 although 200 would be appropriate according to cowboy
  {ok, Req2} = cowboy_req:reply(201, [?JSON_CT_HEADER], EncodedPost, Req),
  {halt, Req2, State}.

%% get posts from crate and return them as JSON
posts_to_json(Req, #{craterl := CraterlClientRef}=State) ->
  Posts = guestbook_post_model:get_posts(CraterlClientRef),
  {jsx:encode(Posts), Req, State}.
