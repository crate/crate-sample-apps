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
