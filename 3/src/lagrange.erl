-module(lagrange).

-export([start/3]).

-import(utils, []).

start(OutputPid, Step, Window) ->
    spawn(fun() ->
             utils:queue_loop(Step,
                              queue:from_list([]),
                              Window,
                              OutputPid,
                              "Lagrange",
                              fun interpolate/2)
          end).

lagrange_polynomial(Points, X) ->
    Xs = lists:map(fun([Xi, _Y]) -> Xi end, Points),
    Sum = lists:foldl(fun({[Xi, Y], Index}, Acc) ->
                         Y * diff_prod(X, Index, Xs) / diff_prod(Xi, Index, Xs) + Acc
                      end,
                      0,
                      lists:zip(Points, lists:seq(1, length(Points)))),
    Sum.

diff_prod(X, SkipInd, Xs) ->
    element(1,
            lists:foldl(fun(Xi, {Acc, Ind}) ->
                           case SkipInd of
                               Ind ->
                                   {Acc, Ind + 1};
                               _ ->
                                   {(X - Xi) * Acc, Ind + 1}
                           end
                        end,
                        {1, 1},
                        Xs)).

interpolate(Step, Points) ->
    Xs = utils:float_range(hd(hd(Points)), hd(lists:last(Points)), Step),
    Ys = lists:map(fun(X) -> lagrange_polynomial(Points, X) end, Xs),
    [Xs, Ys].
