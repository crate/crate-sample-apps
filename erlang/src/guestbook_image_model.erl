%%%-------------------------------------------------------------------
%%% @author mat
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jan 2016 00:50
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