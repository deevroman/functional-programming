-module(period_tests).

-include_lib("eunit/include/eunit.hrl").

find_number_with_max_period_rec_test() ->
  ?assertEqual(period:find_number_with_max_period_rec(), 983).

find_number_with_max_period_fold_test() ->
  ?assertEqual(period:find_number_with_max_period_fold(), 983).

find_number_with_max_period_map_test() ->
  ?assertEqual(period:find_number_with_max_period_map(), 983).
