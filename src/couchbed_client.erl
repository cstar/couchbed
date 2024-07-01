-module(couchbed_client).
-export([start/0]).

start() ->
    {ok, Socket} = gen_tcp:connect("localhost", 1234, [binary, {packet, 0}, {active, false}]),
    gen_tcp:send(Socket, <<"Hello, server!">>),
    % On ne ferme pas le socket ici pour rester en état CLOSE-WAIT
    io:format("Sent data to server. Waiting...~n"),
    timer:sleep(60000),  % Attend pendant un moment pour observer l'état CLOSE-WAIT
    gen_tcp:close(Socket),
    io:format("Client closed the socket~n").
