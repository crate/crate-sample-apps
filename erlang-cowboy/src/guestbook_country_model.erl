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
%%% the model for the countries table
%%% @end
%%%-------------------------------------------------------------------
-module(guestbook_country_model).

%% API
-export([
  prepare_countries/1
]).

-define(SCHEMA, <<"CREATE TABLE IF NOT EXISTS guestbook.countries (
    id STRING PRIMARY KEY,
    name STRING PRIMARY KEY,
    geometry GEO_SHAPE
) WITH (number_of_replicas='0-2')">>).
-define(IMPORT_QUERY, <<"COPY guestbook.countries FROM ?">>).

prepare_countries(CraterlClient) ->
  {ok, Response} = craterl:sql(CraterlClient, ?SCHEMA),
  case craterl_resp:row_count(Response) of
    1 ->
      CountriesFile = list_to_binary(filename:absname("../sql/countries.json")),
      io:format("importing countries from: ~p~n", [CountriesFile]),
      {ok, _ImportResponse} = craterl:sql(CraterlClient, ?IMPORT_QUERY, [CountriesFile]);
    _ -> ok
  end,
  ok.