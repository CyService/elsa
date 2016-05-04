
-module(elsa_kernel_handler_v3).

-export([start/0]).

start() ->
  spawn(fun() -> listen() end).

listen() ->
  {ok, Listen} = gen_tcp:listen(8000, [binary, {packet, line}, {reuseaddr, true}, {active, false}]),
  accept(Listen).

accept(Listen) ->
  {ok, Client} = gen_tcp:accept(Listen),
  spawn(fun() -> handle_conn(Client) end),
  accept(Listen).

handle_conn(Client) ->
  {Service, Name, Version, Thread} = parse_request_line(Client),
  parse_headers(Client, Service, 0),
  parse_headers(Service, Client, 0),
  {_, I, F} = Thread,
  elsa_thread_controller:put(Name, Version, I, F).

parse_request_line(Client) ->
  {ok, Line} = gen_tcp:recv(Client, 0),
  {Name, Version, NewLine} = parse(Line),
  {{Address, Port}, Thread} = elsa_kernel_controller:find_service(Name, Version),
  lager:info("about to connect to ~p", [Address]),
  {ok, Service} = gen_tcp:connect(Address, Port, [binary, {packet, line}, {active, false}, {active, false}]),
  gen_tcp:send(Service, NewLine),
  {Service, Name, Version, Thread}.

parse_headers(Client, Service, ContentLength) ->
  case gen_tcp:recv(Client, 0) of
    {ok, Line = <<"\r\n">>} ->
      lager:info("Sending: ~p", [Line]),
      gen_tcp:send(Service, Line),
      inet:setopts(Client, [{packet, raw}]),
      parse_body(Client, Service, ContentLength, 0);
    {ok, Line = <<"Transfer-Encoding: chunked\r\n">>} ->
      gen_tcp:send(Service, Line),
      parse_headers(Client, Service, chunked);
    {ok, Line = <<"Content-Length: ", N/binary>>} ->
      lager:info("Sending: ~p", [Line]),
      gen_tcp:send(Service, Line),
      parse_headers(Client, Service, content_length(N));
    {ok, Line} ->
      lager:info("Sending: ~p", [Line]),
      gen_tcp:send(Service, Line),
      parse_headers(Client, Service, ContentLength);
    {error, closed} ->
      lager:error("Connection dropped")
  end.

parse_body(Client, Service, chunked, ReadSoFar) ->
  lager:info("Chunked bullshit here"),
  case gen_tcp:recv(Client, 0) of
     {ok, Line = <<"0/r/n">>} ->
      gen_tcp:send(Service, Line);
     {ok, Bin} ->
      lager:info("Chunked bullshit here: ~p", [Bin]),
       gen_tcp:send(Service, Bin),
       parse_body(Client, Service, chunked, ReadSoFar + size(Bin));
     {error, closed} ->
       lager:info("Connection dropped")
   end;
parse_body(Client, Service, ContentLength, ReadSoFar) ->
 lager:info("Reading body length, ~p", [ContentLength]),
 if
   ReadSoFar /= ContentLength ->
     case gen_tcp:recv(Client, 0) of
       {ok, Bin} ->
         gen_tcp:send(Service, Bin),
         parse_body(Client, Service, ContentLength, ReadSoFar + size(Bin));
       {error, closed} ->
         lager:info("Connection dropped")
     end;
   true -> ok
 end.

content_length(Val) ->
  [Num, _] = binary:split(Val, <<"\r">>),
  binary_to_integer(Num).

parse(RequestLine) ->
  [Verb, Rest] = binary:split(RequestLine, <<" ">>),
  lager:info("Verb and Rest: ~p : ~p", [Verb, Rest]),
  [_, Name, Version | Rest2] = binary:split(Rest, <<"/">>, [global]),
  lager:info("Name, Version, and Rest: ~p : ~p : ~p", [Name, Version, Rest2]),
  URL = [Verb,<<" /">>,Rest2],
  Cap = concat_bin(Rest2, <<"">>),
  NewLine = <<Verb/binary, <<" ">>/binary, Cap/binary>>,
  lager:info("Rest: ~p", [NewLine]),
  {Name, Version, NewLine}.

concat_bin([], Bin) -> Bin;
concat_bin([Item | List], Bin) ->
  concat_bin(List, <<Bin/binary, <<"/">>/binary, Item/binary>>).
