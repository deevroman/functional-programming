-module(linear).

-export([start/2]).

-import(utils, []).

start(OutputPid, Step) ->
    spawn(fun() ->
             utils:queue_loop(Step, queue:from_list([]), 2, OutputPid, "Linear", fun interpolate/2)
          end).

interpolate(Step, [[X1, Y1], [X2, Y2]]) ->
    K = (Y2 - Y1) / (X2 - X1),
    B = Y1 - K * X1,
    Xs = utils:float_range(X1, X2, Step),
    Ys = lists:map(fun(X) -> K * X + B end, Xs),
    [Xs, Ys].
