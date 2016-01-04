%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2016 00:38
%%%-------------------------------------------------------------------
-module(guestbook_country_model).
-author("mat").

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