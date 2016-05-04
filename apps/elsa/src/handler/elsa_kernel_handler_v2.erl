
-module(elsa_kernel_handler_v2).

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
  {_, I ,F} = Thread,
  elsa_thread_controller:put(Name, Version, I, F),
  spawn(fun() -> proxy(Service, Client) end),
  proxy(Client, Service).

parse_request_line(Client) ->
  {ok, Line} = gen_tcp:recv(Client, 0),
  {Name, Version, NewLine} = parse(Line),
  {{Address, Port}, Thread} = elsa_kernel_controller:find_service(Name, Version),
  lager:info("about to connect to ~p", [Address]),
  {ok, Service} = gen_tcp:connect(Address, Port, [binary, {packet, raw}, {active, false}, {active, false}]),
  gen_tcp:send(Service, NewLine),
  inet:setopts(Client, [{packet, raw}]),
  {Service, Name, Version, Thread}.

proxy(Source, Destination) ->
  case gen_tcp:recv(Source, 0) of
     {ok, Bin} ->
       lager:info("Bin: ~p", [Bin]),
       gen_tcp:send(Destination, Bin),
       proxy(Source, Destination);
     {error, closed} ->
       lager:info("Connection closed")
   end.

parse(RequestLine) ->
  [Verb, Rest] = binary:split(RequestLine, <<" ">>),
  lager:info("Verb and Rest: ~p : ~p", [Verb, Rest]),
  [_, Name, Version | Rest2] = binary:split(Rest, <<"/">>, [global]),
  lager:info("Name, Version, and Rest: ~p : ~p : ~p", [Name, Version, Rest2]),
  Cap = concat_bin(Rest2, <<"">>),
  NewLine = <<Verb/binary, <<" ">>/binary, Cap/binary>>,
  lager:info("Rest: ~p", [NewLine]),
  {Name, Version, NewLine}.

concat_bin([], Bin) -> Bin;
concat_bin([Item | List], Bin) ->
  concat_bin(List, <<Bin/binary, <<"/">>/binary, Item/binary>>).
