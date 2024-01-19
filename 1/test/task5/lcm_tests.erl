-module(lcm_tests).
-include_lib("eunit/include/eunit.hrl").

lcm_rec_test() ->
  ?assertEqual(lcm:smallest_divisible_rec(), 232792560).

lcm_tail_rec_test() ->
  ?assertEqual(lcm:smallest_divisible_tail_rec(), 232792560).

lcm_reduce_test() ->
  ?assertEqual(lcm:smallest_divisible_reduce(), 232792560).

lcm_inf_list_via_concurrency_map_test() ->
  ?assertEqual(lcm:smallest_divisible_inf_list_via_concurrency_map(), 232792560).

lcm_inf_list_via_concurrency_fold_test() ->
  ?assertEqual(lcm:smallest_divisible_inf_list_via_concurrency_fold(), 232792560).

smallest_divisible_inf_lazy_lists_test() ->
  ?assertEqual(lcm:smallest_divisible_inf_lazy_lists(), 232792560).
