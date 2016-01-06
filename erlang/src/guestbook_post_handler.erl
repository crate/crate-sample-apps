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
%%% http rest handler for the /images endpoint
%%% @end
%%%-------------------------------------------------------------------
-module(guestbook_post_handler).

%% API
-export([
  init/3,
  rest_init/2,
  allowed_methods/2,
  delete_resource/2,
  content_types_provided/2,
  content_types_accepted/2,
  malformed_request/2,
  resource_exists/2
]).

-export([
  post_from_json/2
  ,post_to_json/2
]).

-include("guestbook.hrl").

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {craterl, ClientRef} = lists:keyfind(craterl, 1, Opts),
  {ok, Req, #{craterl => ClientRef}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

delete_resource(Req, #{craterl := ClientRef}=State) ->
  {Id, Req2} = cowboy_req:binding(id, Req),
  case guestbook_post_model:delete_post(ClientRef, Id) of
    ok -> {true, Req2, State};
    not_found -> {true, Req2, State};
    _ -> {false, Req2, State}
  end.

content_types_provided(Req, State) ->
  {[
    {?JSON_CONTENT_TYPE, post_to_json}
  ], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {?JSON_CONTENT_TYPE, post_from_json}
  ], Req, State}.

malformed_request(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  malformed_request(Method, Req2, State).
malformed_request(<<"PUT">>, Req, State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  case Body of
    <<>> ->
      Req3 = guestbook_handlers:error_message(400, <<"Body is required">>, Req2),
      {true, Req3, State};
    _ ->
      try
        case jsx:decode(Body, [return_maps]) of
          %% verify correct body with pattern matching
          #{<<"text">> := Text} = Decoded when is_binary(Text)->
            TooManyKeys = maps:size(Decoded) > 1,
            {TooManyKeys, Req2, State#{payload => Decoded}};
          _ ->
            Req3 = guestbook_handlers:error_message(400, "invalid json", Req2),
            {true, Req3, State}
        end
      catch
        error:badarg ->
          Req5 = guestbook_handlers:error_message(400, "invalid json", Req2),
          {true, Req5, State}
      end
  end;
malformed_request(_OtherMethod, Req, State) ->
  {false, Req, State}.

resource_exists(Req, #{craterl := CrateClientRef}=State) ->
  {Id, Req2} = cowboy_req:binding(id, Req),
  case guestbook_post_model:get_post(CrateClientRef, Id) of
    not_found ->
        Req3 = handle_error(404, Id, Req2),
        {false, Req3, State};
    Post ->
      {true, Req2, State#{post => Post}}
  end.

post_to_json(Req, #{post := Post} = State) ->
  {jsx:encode(Post), Req, State}.

post_from_json(Req, #{craterl := CraterlClientRef, payload := Payload, post := #{<<"id">> := PostId}} = State) ->
  {ok, UpdatedPost} = guestbook_post_model:update_post(CraterlClientRef, PostId, Payload),
  EncodedPost = jsx:encode(UpdatedPost),
  Req2 = cowboy_req:set_resp_body(EncodedPost, Req),
  {true, Req2, State};
post_from_json(Req, State) ->
  {ok, Req2} = cowboy_req:reply(404, Req),
  {halt, Req2, State}.

handle_error(StatusCode, Id, Req) ->
  case StatusCode of
    404 ->
      Message = "Post with id=\"" ++ binary_to_list(Id) ++ "\" not found",
      guestbook_handlers:error_message(404, Message, Req);
    _ -> Req
  end.