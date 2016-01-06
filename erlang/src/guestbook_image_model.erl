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
%%% the model for the image blobs
%%% @end
%%%-------------------------------------------------------------------
-module(guestbook_image_model).

%% API
-export([
  prepare_images/1,
  create_image/2,
  get_images/1,
  get_image/2,
  delete_image/2
]).

-include("guestbook.hrl").

-define(SCHEMA, <<"CREATE BLOB TABLE guestbook_images
WITH (number_of_replicas='0-2')">>).
-define(LIST_STMT, <<"select digest, last_modified from blob.guestbook_images">>).


prepare_images(CraterlClient) ->
  craterl:sql(CraterlClient, ?SCHEMA),
  ok.

create_image(CraterlClient, BinaryImage) ->
  craterl:blob_put(CraterlClient, ?IMAGES_BLOB_TABLE, BinaryImage).

get_images(CraterlClient) ->
  {ok, Response} = craterl:sql(CraterlClient, ?LIST_STMT),
  ColumnNames = craterl_resp:column_names(Response),
  FormatFun = fun (Row) -> maps:from_list(lists:zip(ColumnNames, Row)) end,
  lists:map(FormatFun, craterl_resp:rows(Response)).

get_image(CraterlClient, HexDigest) ->
  craterl:blob_get(CraterlClient, ?IMAGES_BLOB_TABLE, HexDigest).

delete_image(CraterlClient, HexDigest) ->
  craterl:blob_delete(CraterlClient, ?IMAGES_BLOB_TABLE, HexDigest).