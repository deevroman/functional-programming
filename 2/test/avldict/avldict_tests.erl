-module(avldict_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HEIGHT(Node), element(3, Node)).
-define(NIL, {nil, nil, 0, nil, nil}).

balance_check({nil, nil, H, nil, nil}) ->
    H == 0;
balance_check({_, _, H, S, B}) ->
    H1 = ?HEIGHT(S),
    H2 = ?HEIGHT(B),
    balance_check(S)
    and balance_check(B)
    and (-2 < H2 - H1)
    and (H2 - H1 < 2)
    and (H == max(H1, H2) + 1).

insert_all(L) ->
    %% Свойство 1. Дерево должно быть сбалансировано после выполнения любой операции
    R = lists:foldl(fun({K, V}, Acc) ->
                       Cur = avldict:insert(K, V, Acc),
                       ?assertEqual(true, balance_check(Cur)),
                       Cur
                    end,
                    avldict:empty_tree(),
                    L),
    %% Свойство 2. Дерево должно быть бинарным. Т.е. в результате в ЛКП-обхода ключи не убывают
    ?assertEqual(avldict:to_list(R), lists:usort(L)).

empty_tree_test() ->
    ?assertEqual(avldict:empty_tree(), ?NIL).

one_node_test() ->
    T = avldict:insert(1, 2, avldict:empty_tree()),
    Target = {1, 2, 1, ?NIL, ?NIL},
    ?assertEqual(T, Target),
    ?assertEqual(avldict:find(0, T), not_found),
    ?assertEqual(avldict:find(1, T), 2).

two_node_test() ->
    T1 = avldict:insert(1, 1, avldict:empty_tree()),
    T2 = avldict:insert(2, 2, T1),
    Target = {1, 1, 2, ?NIL, {2, 2, 1, ?NIL, ?NIL}},
    ?assertEqual(T2, Target).

simple_tree_test() ->
    L = [{1, 2}, {2, 3}, {4, 5}, {6, 7}, {8, 9}, {10, 11}],
    Target =
        {6,
         7,
         3,
         {2, 3, 2, {1, 2, 1, ?NIL, ?NIL}, {4, 5, 1, ?NIL, ?NIL}},
         {8, 9, 2, ?NIL, {10, 11, 1, ?NIL, ?NIL}}},
    ?assertEqual(avldict:from_list(L), Target).

big_insert_test() ->
    L = [{random:uniform(1_000_000), 0} || _ <- lists:seq(1, 1_000_000)],
    T = avldict:from_list(L),
    balance_check(T),
    ?assertEqual(avldict:to_list(T), lists:usort(L)).

%% Тесты операции удаления из дерева

one_node_remove_test() ->
    T = avldict:from_list([{1, 1}]),
    ?assertEqual(avldict:remove(1, T), avldict:empty_tree()).

simple_node_remove_test() ->
    T = avldict:from_list([{1, 1}, {0, 0}]),
    ?assertEqual(avldict:to_list(
                     avldict:remove(1, T)),
                 [{0, 0}]).

simple_node2_remove_test() ->
    T = avldict:from_list([{1, 1}, {0, 0}, {2, 2}]),
    ?assertEqual(avldict:to_list(
                     avldict:remove(2, T)),
                 [{0, 0}, {1, 1}]),
    ?assertEqual(avldict:to_list(
                     avldict:remove(1, T)),
                 [{0, 0}, {2, 2}]).

simple_node4_remove_test() ->
    T = avldict:from_list([{1, 1}, {0, 0}, {2, 2}, {4, 4}]),
    ?assertEqual(avldict:to_list(
                     avldict:remove(2, T)),
                 [{0, 0}, {1, 1}, {4, 4}]).

simple_not_found_remove_test() ->
    L = [{I, I} || I <- lists:seq(0, 3)],
    T = avldict:from_list(L),
    ?assertEqual(avldict:to_list(
                     avldict:remove(42, T)),
                 L).

remove_all(L, T) ->
    {R, _} =
        lists:foldl(fun({K, _V}, {TAcc, LAcc}) ->
                       CurT = avldict:remove(K, TAcc),
                       [_ | CurL] = LAcc,
                       ?assertEqual(true, balance_check(CurT)),
                       ?assertEqual(lists:usort(CurL), avldict:to_list(CurT)),
                       {CurT, CurL}
                    end,
                    {T, L},
                    L),
    ?assertEqual(R, avldict:empty_tree()).

remove_all_test() ->
    L = [{1, 1}, {2, 2}, {0, 0}, {4, 4}],
    T = avldict:from_list(L),
    remove_all(L, T).

remove_test_case(Size) ->
    Set = sets:from_list([{random:uniform(Size), 0} || _ <- lists:seq(1, Size)]),
    L = sets:to_list(Set),
    T = avldict:from_list(L),
    remove_all(L, T).

small_remove_test() ->
    [remove_test_case(random:uniform(6)) || _ <- lists:seq(1, 10000)].

medium_remove_test() ->
    [remove_test_case(random:uniform(20)) || _ <- lists:seq(1, 10000)].

big_remove_test() ->
    [remove_test_case(random:uniform(100)) || _ <- lists:seq(1, 10000)].

%% Перебор возможных вставок элементов из n-элементов.
%% Хорошее покрытие, но не проверяет поведение при вставке повторяющихся элементов
perms([]) ->
    [[]];
perms(L) ->
    [[H | T] || H <- L, T <- perms(L -- [H])].

perms_n(X) ->
    lists:map(fun(L) -> insert_all(L) end, perms([{I, 0} || I <- lists:seq(1, X)])).

all_inserts_perms_test_() ->
    [?_test(perms_n(1)),
     ?_test(perms_n(2)),
     ?_test(perms_n(3)),
     ?_test(perms_n(4)),
     ?_test(perms_n(5)),
     ?_test(perms_n(6)),
     ?_test(perms_n(7)),
     ?_test(perms_n(8)),
     ?_test(perms_n(9))].

%% Чтобы проверить работу с дубликатами проверим все размещения с возвратом
%% Их n^n поэтому проверяет только для 7 элементов в словаре
%% Зато это полное покрытие для всевозможных вставок в дерево
placement_with_repetitions4_test() ->
    L = lists:seq(1, 4),
    A = [[{X, 0}, {Y, 0}, {Z, 0}, {Q, 0}] || X <- L, Y <- L, Z <- L, Q <- L],
    lists:map(fun insert_all/1, A).

placement_with_repetitions5_test() ->
    L = lists:seq(1, 5),
    A = [[{X, 0}, {Y, 0}, {Z, 0}, {Q, 0}, {W, 0}] || X <- L, Y <- L, Z <- L, Q <- L, W <- L],
    lists:map(fun insert_all/1, A).

placement_with_repetitions6_test() ->
    L = lists:seq(1, 6),
    A = [[{X, 0}, {Y, 0}, {Z, 0}, {Q, 0}, {W, 0}, {E, 0}]
         || X <- L, Y <- L, Z <- L, Q <- L, W <- L, E <- L],
    lists:map(fun insert_all/1, A).

placement_with_repetitions7_test() ->
    L = lists:seq(1, 7),
    A = [[{X, 0}, {Y, 0}, {Z, 0}, {Q, 0}, {W, 0}, {E, 0}, {R, 0}]
         || X <- L, Y <- L, Z <- L, Q <- L, W <- L, E <- L, R <- L],
    lists:map(fun insert_all/1, A).

%% Тесты дополнительных функций, которые должен поддерживать словарь
map_tree_test() ->
    L = [{I, 0} || I <- lists:seq(1, 20)],
    T = avldict:from_list(L),
    T2 = avldict:map_tree(T, fun({K, V}) -> {K * 2, V} end),
    L2 = lists:map(fun({K, V}) -> {K * 2, V} end, L),
    ?assertEqual(L2, avldict:to_list(T2)).

filter_tree_test() ->
    L = [{I, 0} || I <- lists:seq(1, 20)],
    T = avldict:from_list(L),
    T2 = avldict:filter_tree(T, fun({K, _V}) -> K rem 2 =:= 0 end),
    L2 = lists:filter(fun({K, _V}) -> K rem 2 =:= 0 end, L),
    ?assertEqual(L2, avldict:to_list(T2)).

%% Для свёрток выбрана конкатенация, ибо она не коммутативна
%% Следовательно будет проверен порядок элементов
foldl_tree_test() ->
    L = [{I, 0} || I <- lists:seq(1, 20)],
    T = avldict:from_list(L),
    R = avldict:foldl(T, fun(Acc, {K, _V}) -> Acc ++ "|" ++ integer_to_list(K) end, ""),
    R2 = lists:foldl(fun({K, _V}, Acc) -> Acc ++ "|" ++ integer_to_list(K) end, "", L),
    ?assertEqual(R, R2).

foldr_tree_test() ->
    L = [{I, 0} || I <- lists:seq(1, 20)],
    T = avldict:from_list(L),
    R = avldict:foldr(T, fun(Acc, {K, _V}) -> Acc ++ "|" ++ integer_to_list(K) end, ""),
    R2 = lists:foldr(fun({K, _V}, Acc) -> Acc ++ "|" ++ integer_to_list(K) end, "", L),
    ?assertEqual(R, R2).

%% Свойство 3. AVL-Dict должен быть моноидом
%% А значит, должен быть единичный элемент...

monoid_neutral_elem_case(TSize) ->
    T = avldict:from_list([{random:uniform(30), 0} || _ <- lists:seq(1, TSize)]),
    R = avldict:merge(T, avldict:empty_tree()),
    ?assertEqual(avldict:to_list(T), avldict:to_list(R)),
    R2 = avldict:merge(
             avldict:empty_tree(), T),
    ?assertEqual(avldict:to_list(T), avldict:to_list(R2)).

monoid_neutral_elem_test() ->
    [monoid_neutral_elem_case(random:uniform(1000) - 1) || _ <- lists:seq(1, 10000)].

%% ... и должна существовать ассоциативная операция умножения,
%% которой является слияния деревьев

monoid_assoc_test_case(ASize, BSize, CSize) ->
    A = avldict:from_list([{random:uniform(30), random:uniform(100)}
                           || _ <- lists:seq(1, ASize)]),
    B = avldict:from_list([{random:uniform(30), random:uniform(100)}
                           || _ <- lists:seq(1, BSize)]),
    C = avldict:from_list([{random:uniform(30), random:uniform(100)}
                           || _ <- lists:seq(1, CSize)]),

    R1 = avldict:merge(
             avldict:merge(A, B), C),
    R2 = avldict:merge(A, avldict:merge(B, C)),
    ?assertEqual(avldict:to_list(R1), avldict:to_list(R2)).

monoid_assoc_small_test() ->
    [monoid_assoc_test_case(random:uniform(10) - 1,
                            random:uniform(10) - 1,
                            random:uniform(10) - 1)
     || _ <- lists:seq(1, 10000)].

monoid_assoc_medium_test() ->
    [monoid_assoc_test_case(random:uniform(50) - 1,
                            random:uniform(50) - 1,
                            random:uniform(50) - 1)
     || _ <- lists:seq(1, 10000)].

monoid_assoc_big_test() ->
    [monoid_assoc_test_case(random:uniform(100) - 1,
                            random:uniform(100) - 1,
                            random:uniform(100) - 1)
     || _ <- lists:seq(1, 10000)].
