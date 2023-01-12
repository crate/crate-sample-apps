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
%%% http handler for the /search endpoint
%%% @end
%%%-------------------------------------------------------------------
-module(guestbook_search_handler).

-behaviour(cowboy_http_handler).

-include("guestbook.hrl").

%% API
-export([
      init/3,
      handle/2,
      terminate/3
      ]).

-include("guestbook_cors.hrl").

-record(state, {
  craterl :: craterl:craterl_client_ref()
}).

-define(SEARCH_TERM_PARAM_NAME, <<"query_string">>).

init(_, Req, Opts) ->
  {craterl, CraterlClientRef} = lists:keyfind(craterl, 1, Opts),
  Req2 = addCORSHeaders(Req),
  {ok, Req2, #state{craterl=CraterlClientRef}}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  handle(Method, Req2, State).
handle(<<"OPTIONS">>, Req, State) ->
  {ok, cowboy_req:set_resp_body("OK", Req), State};
handle(<<"POST">>, Req, #state{craterl=CraterlClientRef}=State) ->
  Req4 = case get_search_term(Req) of
    {ok, SearchTerm, Req2} ->
      Posts = guestbook_post_model:search_posts(CraterlClientRef, SearchTerm),
      {ok, Req3} = cowboy_req:reply(
        200,
        [?JSON_CT_HEADER],
        jsx:encode(Posts),
        Req2
      ),
      Req3;
    {error, Req2} ->
      guestbook_handlers:error_response(400, <<"invalid json">>, Req2)
  end,
  {ok, Req4, State};
handle(_OtherMethod, Req, State) ->
  Req2 = guestbook_handlers:error_response(405, <<"method not supported">>, Req),
  {ok, Req2, State}.

terminate(_, _, _) ->
  ok.

%%% private
get_search_term(Req) ->
  case cowboy_req:body(Req) of
    {ok, <<>>, Req2} -> {error, Req2};
    {ok, Body, Req2} ->
      try
        case jsx:decode(Body, [return_maps]) of
          #{?SEARCH_TERM_PARAM_NAME := SearchTerm} when is_binary(SearchTerm) ->
            {ok, SearchTerm, Req2};
          _ ->
            {error, Req2}
        end

      catch
        error:badarg ->
          {error, Req2}
      end
  end.
