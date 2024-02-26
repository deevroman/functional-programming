-module(gauss).

-export([start/3]).

start(OutputPid, Step, Window) ->
    spawn(fun() -> loop(OutputPid, Step, Window) end).

loop(Step, Window, OutputPid) ->
    receive
        Message ->
            io:format("Gauss ~p~n", [Message])
    end,
    loop(Step, Window, OutputPid).
