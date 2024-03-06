-module(utils).

-export([queue_loop/6]).
-export([float_range/3]).

queue_loop(Step, Points, Window, OutputPid, MethodName, InterpolateFun) ->
    NewPoints =
        receive
            {new_point, Point, _} ->
                case queue:len(Points) of
                    X when X == Window ->
                        process_new(queue:drop(
                                        queue:in(Point, Points)),
                                    Step,
                                    OutputPid,
                                    MethodName,
                                    InterpolateFun);
                    0 ->
                        queue_loop(Step,
                                   queue:in(Point, Points),
                                   Window,
                                   OutputPid,
                                   MethodName,
                                   InterpolateFun);
                    X when X =< Window - 1 ->
                        process_new(queue:in(Point, Points),
                                    Step,
                                    OutputPid,
                                    MethodName,
                                    InterpolateFun)
                end;
            {stop, _, _} ->
                exit(ok);
            Msg ->
                io:format("Unknown message: ~p~n", [Msg]),
                queue_loop(Step, Points, Window, OutputPid, MethodName, InterpolateFun)
        end,
    queue_loop(Step, NewPoints, Window, OutputPid, MethodName, InterpolateFun).

process_new(Points, Step, OutputPid, MethodName, InterpolateFun) ->
    Result = InterpolateFun(Step, queue:to_list(Points)),
    OutputPid ! {result, {MethodName, Result}, self()},
    Points.

float_range(First, Last, Step) ->
    IntervalsCnt = trunc((Last - First) / Step),
    [First + Step * I || I <- lists:seq(1, IntervalsCnt)].
