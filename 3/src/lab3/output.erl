-module(output).

-export([start/0]).

start() ->
    spawn(fun loop/0).

loop() ->
    receive
        {result, {Name, [Xs, Ys]}, _} ->
            io:format("~p~n", [Name]),
            print_numbers(Xs),
            print_numbers(Ys);
        Msg ->
            io:format("~p~n", [Msg])
    end,
    loop().

print_numbers(Nums) ->
    io:format("~s~n",
              [lists:join("\t",
                          lists:map(fun(N) -> erlang:float_to_list(float(N), [{decimals, 2}]) end,
                                    Nums))]).
