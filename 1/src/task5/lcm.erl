-module(lcm).

%% API
-export([smallest_divisible_tail_rec/0]).
-export([smallest_divisible_tail_rec/1]).
-export([smallest_divisible_rec/0]).
-export([smallest_divisible_reduce/0]).
-export([smallest_divisible_inf_list_via_concurrency_map/0]).
-export([smallest_divisible_inf_list_via_concurrency_fold/0]).
-export([smallest_divisible_inf_lazy_lists/0]).


gcd(0, Y) -> Y;
gcd(X, 0) -> X;
gcd(X, Y) when X > Y -> gcd(Y, X);
gcd(X, Y) -> gcd(Y rem X, X).

lcm(1, Acc) -> Acc;
lcm(N, Acc) -> lcm(N - 1, (Acc * N) div gcd(Acc, N)).

lcm_rec(1) -> 1;
lcm_rec(N) -> N div gcd(N, lcm_rec(N - 1)) * lcm_rec(N - 1).

smallest_divisible_rec() -> lcm_rec(20).

smallest_divisible_tail_rec(N) -> lcm(N, 1).
smallest_divisible_tail_rec() -> lcm(20, 1).

lcm_two(A, B) -> A div gcd(A, B) * B.

smallest_divisible_reduce() ->
  lists:foldl(fun(Elem, Acc) -> lcm_two(Elem, Acc) end, 1, lists:seq(1, 21)).

%% infinite like via concurrency map

map_worker() ->
  receive
    {Pid, X} -> Pid ! {X, lcm(X, 1)}
  end,
  map_worker().

smallest_divisible_inf_list_via_concurrency_map() ->
  WorkerPid = spawn(fun() -> map_worker() end),
  Arg = 21,
  L = lists:seq(1, Arg),
  [WorkerPid ! {self(), Msg} || Msg <- L],
  lists:nth(Arg, lists:map(fun(X) -> receive {X, Res} -> Res end end, L)).

%% infinite like via concurrency fold

fold_worker() ->
  receive
    {Pid, X, Acc} -> Pid ! {X, lcm_two(X, Acc)}
  end,
  fold_worker().

run(WorkerPid, N) ->
  receive
    {N, Res} -> Res;
    {It, Res} -> WorkerPid ! {self(), It + 1, Res}, run(WorkerPid, N)
  end.

smallest_divisible_inf_list_via_concurrency_fold() ->
  WorkerPid = spawn(fun() -> fold_worker() end),
  WorkerPid ! {self(), 1, 1},
  run(WorkerPid, 21).

%% infinite like via lazy lists

naturals() -> naturals_from(1).
naturals_from(N) -> [N | fun() -> naturals_from(N + 1) end].

take(0, _) -> [];
take(N, [H | LazyT]) -> [H | take(N - 1, LazyT())].

map(_, []) -> [];
map(F, [H | T]) -> [F(H) | fun() -> map(F, T()) end].

foldl(Acc, _, []) -> [Acc];
foldl(Acc, F, [H | T]) -> [F(Acc, H) | fun() -> foldl(F(Acc, H), F, T()) end].

smallest_divisible_inf_lazy_lists() ->
  X = 21,
  lists:nth(X, take(X, foldl(1, fun(A, B) -> lcm_two(A, B) end, naturals()))).