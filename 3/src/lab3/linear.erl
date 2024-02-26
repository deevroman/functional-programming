-module(linear).

-export([start/2]).

start(OutputPid, Step) ->
    spawn(fun() -> loop(Step, queue:from_list([]), OutputPid) end).

float_range(First, Last, Step) ->
    float_range_helper(First, Last, Step, []).

float_range_helper(Current, Last, _Step, Acc) when Current > Last ->
    lists:reverse(Acc);
float_range_helper(Current, Last, Step, Acc) ->
    float_range_helper(Current + Step, Last, Step, [Current | Acc]).

interpolate(Step, [[X1, Y1], [X2, Y2]]) ->
    K = (Y2 - Y1) / (X2 - X1),
    B = Y1 - K * X1,
    Xs = float_range(X1, X2, Step),
    Ys = lists:map(fun(X) -> K * X + B end, Xs),
    [Xs, Ys].

loop(Step, Points, OutputPid) ->
    NewPoints =
        receive
            {new_point, Point, _} ->
                case queue:len(Points) of
                    2 ->
                        process_new(queue:drop(
                                        queue:in(Point, Points)),
                                    Step,
                                    OutputPid);
                    1 ->
                        process_new(queue:in(Point, Points), Step, OutputPid);
                    0 ->
                        loop(Step, queue:in(Point, Points), OutputPid)
                end;
            {stop, _} ->
                exit(ok);
            _Msg ->
                io:format("~p~n", [_Msg]),
                loop(Step, Points, OutputPid)
        end,
    loop(Step, NewPoints, OutputPid).

process_new(Points, Step, OutputPid) ->
    Result = interpolate(Step, queue:to_list(Points)),
    OutputPid ! {result, {"Linear interpolation", Result}, self()},
    Points.
