-module(lcm).

%% API
-export([smallest_divisible_tail_rec/0]).
-export([smallest_divisible_tail_rec/1]).
-export([smallest_divisible_reduce/0]).


gcd(0, Y) -> Y;
gcd(X, 0) -> X;
gcd(X, Y) when X > Y -> gcd(Y, X);
gcd(X, Y) -> gcd(Y rem X, X).

lcm(1, Acc) -> Acc;
lcm(N, Acc) -> lcm(N - 1, (Acc * N) div gcd(Acc, N)).

smallest_divisible_tail_rec(N) -> lcm(N, 1).

smallest_divisible_tail_rec() -> lcm(20, 1).

smallest_divisible_reduce() ->
  lists:foldl(fun(Elem, Acc) -> Acc * Elem div gcd(Acc, Elem) end, 1, lists:seq(1, 21)).
