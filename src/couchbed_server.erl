-module(couchbed_server).
-export([start/0, loop/1]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    accept(ListenSocket).

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> loop(Socket) end),
    accept(ListenSocket).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Received: ~p~n", [Data]),
            gen_tcp:close(Socket),  % Ferme la connexion après avoir reçu les données
            io:format("Socket closed by server~n");
        {error, closed} ->
            io:format("Client closed the connection~n")
    end.
