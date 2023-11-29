-module(period).

%% API
-export([find_number_with_max_period/0]).
-export([find_number_with_max_period_fold/0]).
-export([find_number_with_max_period_tail_rec/0]).

rec_div_five(N) ->
  case N rem 5 of
    0 -> rec_div_five(N div 5);
    _ -> N
  end.

rec_div_two(N) ->
  case N rem 2 of
    0 -> rec_div_two(N div 2);
    _ -> N
  end.

div_five_and_two(N) -> rec_div_five(rec_div_two(N)).

try_period_len(N, _, _) when N == 1 -> 0;

try_period_len(N, Divider, Power) when N > 1 ->
  case Divider rem N of
    1 -> Power;
    _ -> try_period_len(N, Divider * 10, Power + 1)
  end.

get_period(X) when X == 0 -> 0;

get_period(X) -> try_period_len(div_five_and_two(X), 10, 1).

find_number_with_max_period(N) ->
  Arr = lists:map(fun(X) -> get_period(X) end, lists:seq(1, N)),
  string:str(Arr, [lists:max(Arr)]).

%%% №1
find_number_with_max_period() ->
  find_number_with_max_period(1000).

pair_max(X, XIndex, Y, YIndex) ->
  case X > Y of
    true -> {X, XIndex};
    false -> {Y, YIndex}
  end.

find_number_with_max_period_fold(N) ->
  element(2, lists:foldl(
    fun(Elem, Acc) -> pair_max(element(1, Acc), element(2, Acc), get_period(Elem), Elem) end,
    {1, 0},
    lists:seq(1, N)
  )).

%%% №2
find_number_with_max_period_fold() ->
  find_number_with_max_period_fold(1000).

get_max_period_tail_rec(Index, _) when Index == 1 -> {1, 0};

get_max_period_tail_rec(Index, Acc) when Index > 1 ->
  {Value, I} = get_max_period_tail_rec(Index - 1, Acc),
  pair_max(Value, I, get_period(Index), Index).

find_number_with_max_period_tail_rec(N) ->
  element(2, get_max_period_tail_rec(N, {1, 0})).

%%% №3
find_number_with_max_period_tail_rec() ->
  find_number_with_max_period_tail_rec(1000).
