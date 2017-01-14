-module(fav1).
-export([test/0]).

test() ->
    Pid = spawn(fun universal_server/0),
    Pid ! {become, fun factorial_server/0},
    Pid ! {self(), 50},
    receive
        X ->
            io:format("~w~n", [X])
    end.

universal_server() ->
    receive
        {become, F} ->
            F()
    end.

factorial_server() ->
    receive
        {From, N} ->
            From ! factorial(N),
            factorial_server()
    end.

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
