-module(lcm_tests).
-include_lib("eunit/include/eunit.hrl").

lcm_rec_test() ->
  ?assertEqual(lcm:smallest_divisible_rec(), 232792560).

lcm_tail_rec_test() ->
  ?assertEqual(lcm:smallest_divisible_tail_rec(), 232792560).

lcm_reduce_test() ->
  ?assertEqual(lcm:smallest_divisible_reduce(), 232792560).
