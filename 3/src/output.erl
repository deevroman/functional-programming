-module(output).

-export([start/0]).

start() ->
    spawn(fun loop/0).

loop() ->
    receive
        {result, {Name, [Xs, Ys]}, _} ->
            io:format("~s:~n", [Name]),
            print_numbers(Xs),
            print_numbers(Ys);
        {stop, _, _} ->
            exit(ok);
        Msg ->
            io:format("Unknown message: ~p~n", [Msg])
    end,
    loop().

print_numbers(Numbers) ->
    io:format("~s~n",
              [lists:join("\t",
                          lists:map(fun(N) -> erlang:float_to_list(float(N), [{decimals, 3}]) end,
                                    Numbers))]).
