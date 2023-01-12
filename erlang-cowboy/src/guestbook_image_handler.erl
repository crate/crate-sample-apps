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
%%% http handler for the /image/:id endpoint
%%%
%%% this is implemented as simple cowboy_http_handler behaviour
%%% which is much simpler, but does not provide all the error handling goodies
%%% that the rest handler does
%%% @end
%%%-------------------------------------------------------------------
-module(guestbook_image_handler).

-behaviour(cowboy_http_handler).

-include("guestbook.hrl").

%% API
-export([init/3, handle/2, terminate/3]).

-record(state, {
  craterl :: craterl:craterl_client_ref()
}).

init(_, Req, Opts) ->
  {craterl, CraterlClientRef} = lists:keyfind(craterl, 1, Opts),
  {ok, Req, #state{craterl=CraterlClientRef}}.

handle(Req, #state{craterl=CraterlClientRef} = State) ->
  {HexDigest, Req2} = cowboy_req:binding(digest, Req),
  case cowboy_req:method(Req2) of
    {<<"GET">>, Req3} ->
      case guestbook_image_model:get_image(CraterlClientRef, HexDigest) of
        {ok, DataFun} ->
          {ok, Req4} = cowboy_req:chunked_reply(200, [?GIF_CT_HEADER], Req3),
          send_blob_chunked(DataFun, Req4),
          {ok, Req4, State};
        {error, StatusCode} when is_integer(StatusCode) ->
          Req4 = handle_error(StatusCode, HexDigest, Req3),
          {ok, Req4, State};
        {error, _Reason} ->
          cowboy_req:reply(500, Req3)
      end;
    {<<"DELETE">>, Req3} ->
      case guestbook_image_model:delete_image(CraterlClientRef, HexDigest) of
        ok ->
          {ok, Req4} = cowboy_req:reply(204, Req3),
          {ok, Req4, State};
        {error, StatusCode} when is_integer(StatusCode) ->
          Req4 = handle_error(StatusCode, HexDigest, Req3),
          {ok, Req4, State};
        {error, _Reason} ->
          cowboy_req:reply(500, Req3)
      end
  end.

send_blob_chunked(DataFun, Req) ->
  case DataFun() of
    {ok, Data} when is_binary(Data) ->
      case cowboy_req:chunk(Data, Req) of
        ok -> send_blob_chunked(DataFun, Req);
        {error, Reason} ->
          io:format("error: ~p on sending chunks~n", [Reason]),
          ok
      end;
    {ok, done} -> ok
  end,
  ok.

terminate(_, _, _) ->
  ok.

handle_error(StatusCode, HexDigest, Req) ->
  Req2 = case StatusCode of
           404 -> guestbook_handlers:error_message(404, "Image with digest=\"" ++ binary_to_list(HexDigest) ++ "\" not found", Req);
           _ -> Req
         end,
  {ok, Req3} = cowboy_req:reply(StatusCode, Req2),
  Req3.
