-module(period_tests).

-include_lib("eunit/include/eunit.hrl").

find_number_with_max_period_test() ->
  ?assertEqual(period:find_number_with_max_period(), 983).

find_number_with_max_period_tail_rec_test() ->
  ?assertEqual(period:find_number_with_max_period_tail_rec(), 983).

find_number_with_max_period_fold_test() ->
  ?assertEqual(period:find_number_with_max_period_fold(), 983).

