-module(gauss).

-export([start/3]).

-import(utils, []).

start(OutputPid, Step, Window) ->
    spawn(fun() ->
             utils:queue_loop(Step,
                              queue:from_list([]),
                              Window,
                              OutputPid,
                              "Gauss",
                              fun interpolate/2)
          end).

new_matrix(N, M) ->
    lists:map(fun(_) -> [0 || _ <- lists:seq(1, M)] end, lists:seq(1, N)).

get_elem_matrix(M, X, Y) ->
    lists:nth(Y + 1, lists:nth(X + 1, M)).

set_elem_matrix(M, X, Y, NewElem) ->
    lists:sublist(M, X)
    ++ [lists:sublist(
            lists:nth(X + 1, M), Y)
        ++ [NewElem]
        ++ lists:nthtail(Y + 1, lists:nth(X + 1, M))]
    ++ lists:nthtail(X + 1, M).

factorial(X) when X > 1 ->
    X * factorial(X - 1);
factorial(_) ->
    1.

get_x(Points, I) ->
    hd(lists:nth(I + 1, Points)).

get_y(Points, I) ->
    hd(tl(lists:nth(I + 1, Points))).

gauss_t(T, 1) ->
    T;
gauss_t(T, N) ->
    lists:foldl(fun(I, Acc) -> Acc * (T + math:pow(-1, I) * ((I + 1) div 2)) end,
                T,
                lists:seq(1, N - 1)).

gauss_polynomial(Points, X) ->
    N = length(Points),
    Matrix = new_matrix(N, N),
    Matrix1 =
        lists:foldl(fun(I, Acc) -> set_elem_matrix(Acc, I, 0, get_y(Points, I)) end,
                    Matrix,
                    lists:seq(0, N - 1)),
    Matrix2 =
        lists:foldl(fun(I, Acc) ->
                       lists:foldl(fun(J, JAcc) ->
                                      set_elem_matrix(JAcc,
                                                      J,
                                                      I,
                                                      get_elem_matrix(JAcc, J + 1, I - 1)
                                                      - get_elem_matrix(JAcc, J, I - 1))
                                   end,
                                   Acc,
                                   lists:seq(0, N - 2))
                    end,
                    Matrix1,
                    lists:seq(1, N - 1)),
    Base = get_elem_matrix(Matrix2, N div 2, 0),
    T = (X - get_x(Points, N div 2)) / (get_x(Points, 1) - get_x(Points, 0)),
    Result =
        lists:foldl(fun(I, Acc) ->
                       Acc
                       + gauss_t(T, I) * get_elem_matrix(Matrix2, (N - I) div 2, I) / factorial(I)
                    end,
                    0,
                    lists:seq(1, N - 1)),
    Base + Result.

interpolate(Step, Points) ->
    Xs = utils:float_range(hd(hd(Points)), hd(lists:last(Points)), Step),
    Ys = lists:map(fun(X) -> gauss_polynomial(Points, X) end, Xs),
    [Xs, Ys].
