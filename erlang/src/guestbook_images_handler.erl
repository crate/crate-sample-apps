-module(guestbook_images_handler).

%% API
-export([
  init/3,
  rest_init/2,
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  image_from_json/2,
  images_to_json/2,
  malformed_request/2,
  resource_exists/2
]).

-include("guestbook.hrl").

init({_Transport, _Type}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  {craterl, ClientRef} = lists:keyfind(craterl, 1, Opts),
  {ok, Req, #{craterl => ClientRef}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) -> {
  [
    {?JSON_CONTENT_TYPE, image_from_json}
  ],
  Req, State
}.

content_types_provided(Req, State) ->
  {[
    {?JSON_CONTENT_TYPE, images_to_json}
  ], Req, State}.

malformed_request(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  malformed_request(Method, Req2, State).
malformed_request(<<"POST">>, Req, State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  case Body of
    <<>> ->
      Req3 = guestbook_handlers:error_message(400, "body is required", Req2),
      {true, Req3, State};
    _ ->
      try
        case jsx:decode(Body, [return_maps]) of
          %% verify correct body with pattern matching
          #{<<"blob">> := Base64Blob} when is_binary(Base64Blob) ->
            {false, Req2, State#{blob => Base64Blob}};
          Invalid ->
            io:format("invalid payload: ~p~n", [Invalid]),
            Req3 = guestbook_handlers:error_message(400, "invalid json", Req2),
            {true, Req3, State}
        end
      catch
        error:badarg ->
          Req4 = guestbook_handlers:error_message(400, "invalid json", Req2),
          {true, Req4, State}
      end
  end;
malformed_request(_, Req, State) ->
  {false, Req, State}.

resource_exists(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  resource_exists(Method, Req2, State).
resource_exists(<<"GET">>, Req, State) ->
  %% collection of images always exists
  {true, Req, State};
resource_exists(<<"POST">>, Req, #{craterl := CraterlClientRef, blob := Base64Blob}=State) ->
  BinaryImage = base64:decode(Base64Blob),
  %% find out if image already exists by actually uploading it
  case guestbook_image_model:create_image(CraterlClientRef, BinaryImage) of
    {ok, {created, HashDigest}} ->
      {false, Req, State#{digest => HashDigest}};
    {error, {already_exists, HashDigest}} ->
      Req2 = guestbook_handlers:error_response(409, "Image with digest=\"" ++ binary_to_list(HashDigest) ++ "\" already exists", Req),
      {halt, Req2, State}
  end.

image_from_json(Req, #{digest := HashDigest} = State) ->
  Response = jsx:encode(
    #{
      url => <<"/image/", HashDigest/binary>>,
      digest => HashDigest
    }
  ),
  {ok, Req2} = cowboy_req:reply(201, [?JSON_CT_HEADER], Response, Req),
  {halt, Req2, State}.

images_to_json(Req, #{craterl := CraterlClientRef} = State) ->
  Images = guestbook_image_model:get_images(CraterlClientRef),
  {jsx:encode(Images), Req, State}.