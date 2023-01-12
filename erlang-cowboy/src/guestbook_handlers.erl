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
%%% common methods for all http handlers
%%% @end
%%%-------------------------------------------------------------------
-module(guestbook_handlers).

%% API
-export([
  error_message/3,
  error_response/3
]).

-include("guestbook.hrl").

%% create an commonly formatted error message
%% and set it to the response body
%% from an error code and a message
-spec error_message(Code :: integer(), Message :: list()|binary(), Req :: cowboy_req:req()) -> cowboy_req:req().
error_message(Code, Message, Req) when is_list(Message) ->
  error_message(Code, list_to_binary(Message), Req);
error_message(Code, Message, Req) when is_integer(Code), is_binary(Message) ->
  EncodedResponse = jsx:encode(#{
    status => Code,
    error => Message
  }),
  cowboy_req:set_resp_body(EncodedResponse, Req).

%% create a commonly formatted error message and send it to the client
-spec error_response(Code :: integer(), Message :: list()|binary(), Req :: cowboy_req:req()) -> cowboy_req:req().
error_response(Code, Message, Req) ->
  Req2 = error_message(Code, Message, Req),
  {ok, Req3} = cowboy_req:reply(Code, [?JSON_CT_HEADER], Req2),
  Req3.