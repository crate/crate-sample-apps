-module(guestbook_handlers).

%% API
-export([
  error_message/3,
  error_response/3
]).

-include("guestbook.hrl").

error_message(Code, Message, Req) when is_list(Message) ->
  error_message(Code, list_to_binary(Message), Req);
error_message(Code, Message, Req) when is_integer(Code), is_binary(Message) ->
  EncodedResponse = jsx:encode(#{
    status => Code,
    error => Message
  }),
  cowboy_req:set_resp_body(EncodedResponse, Req).

error_response(Code, Message, Req) ->
  Req2 = error_message(Code, Message, Req),
  {ok, Req3} = cowboy_req:reply(Code, [?JSON_CT_HEADER], Req2),
  Req3.