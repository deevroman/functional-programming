-module(avldict).

-export([empty_tree/0, insert/3, remove/2, find/2, from_list/1, to_list/1, merge/2,
         map_tree/2, filter_tree/2, foldl/3, foldr/3, equal_tree/2]).

-define(KEY(Node), element(1, Node)).
-define(VALUE(Node), element(2, Node)).
-define(HEIGHT(Node), element(3, Node)).
-define(NIL, {nil, nil, 0, nil, nil}).

%% Узел дерева представляется как кортеж вида: {Key, Value, H, S, B}
%% Key — уникальный ключ
%% Value — значение ключа
%% H — высота узла. Считается от самого глубокого листа
%% S — Левый ребёнок узла (Smaller, ибо значение должно быть меньше, чем в текущем)
%% B — Правый ребёнок узла (Bigger, ибо значение должно быть больше, чем в текущем)

empty_tree() ->
    ?NIL.

find(_Key, ?NIL) ->
    not_found;
find(Key, {Key, Value, _, _, _}) ->
    {Key, Value};
find(Key, {Key1, _, _, Smaller, _}) when Key < Key1 ->
    find(Key, Smaller);
find(Key, {Key1, _, _, _, Bigger}) when Key > Key1 ->
    find(Key, Bigger).

insert(NewKey, NewVal, ?NIL) ->
    {NewKey, NewVal, 1, ?NIL, ?NIL};
insert(NewKey, NewVal, {Key, _Val, H, Smaller, Bigger}) when NewKey == Key ->
    {NewKey, NewVal, H, Smaller, Bigger};
insert(NewKey, NewVal, {Key, Val, H, S, B}) when NewKey < Key ->
    NewSmaller = insert(NewKey, NewVal, S),
    balance({Key, Val, max(?HEIGHT(NewSmaller) + 1, H), NewSmaller, B});
insert(NewKey, NewVal, {Key, Val, H, S, B}) when NewKey > Key ->
    NewBigger = insert(NewKey, NewVal, B),
    balance({Key, Val, max(?HEIGHT(NewBigger) + 1, H), S, NewBigger}).

balance({_, _, _, S, B} = T) when abs(?HEIGHT(S) - ?HEIGHT(B)) =/= 2 ->
    T;
balance({Key, Val, _H, {KeyS, ValS, _HS, SS, SB}, B})
    when ?HEIGHT(SS) == ?HEIGHT(B) + 1 ->
    NewHB = max(?HEIGHT(B), ?HEIGHT(SB)) + 1,
    {KeyS, ValS, max(NewHB, ?HEIGHT(SS)) + 1, SS, {Key, Val, NewHB, SB, B}};
balance({Key, Val, _H, S, {KeyB, ValB, _HB, BS, BB}})
    when ?HEIGHT(BB) == ?HEIGHT(S) + 1 ->
    NewHS = max(?HEIGHT(S), ?HEIGHT(BS)) + 1,
    {KeyB, ValB, max(NewHS, ?HEIGHT(BB)) + 1, {Key, Val, NewHS, S, BS}, BB};
balance({Key, Val, _H, {KeyS, ValS, _HS, SS, {KeySB, ValSB, HSB, SBS, SBB}}, B})
    when HSB == ?HEIGHT(B) + 1 ->
    HSS = ?HEIGHT(SS),
    {KeySB, ValSB, HSS + 2, {KeyS, ValS, HSS + 1, SS, SBS}, {Key, Val, HSS + 1, SBB, B}};
balance({Key, Val, _H, S, {KeyB, ValB, _HB, {KeyBS, ValBS, HBS, BSS, BSB}, BB}})
    when HBS == ?HEIGHT(S) + 1 ->
    HBB = ?HEIGHT(BB),
    {KeyBS, ValBS, HBB + 2, {Key, Val, HBB + 1, S, BSS}, {KeyB, ValB, HBB + 1, BSB, BB}}.

remove_min({Key, Val, _, ?NIL, B}) ->
    {{Key, Val, nil, nil, nil}, B, true};
remove_min({Key, Val, _, S, ?NIL}) ->
    {S, {Key, Val, 1, ?NIL, ?NIL}, true};
remove_min({Key, Val, _, Smaller, {KB, VB, HB, SB, BB} = Bigger}) ->
    {Min, NewS, IsLastCall} = remove_min(Smaller),
    case {IsLastCall, NewS, BB, HB} of
        {true, ?NIL, ?NIL, 2} ->
            {Min,
             {?KEY(SB), ?VALUE(SB), 2, {Key, Val, 1, ?NIL, ?NIL}, {KB, VB, 1, ?NIL, ?NIL}},
             false};
        {true, ?NIL, _, _} ->
            {Min,
             {KB,
              VB,
              max(?HEIGHT(SB) + 1, ?HEIGHT(BB)) + 1,
              {Key, Val, ?HEIGHT(SB) + 1, ?NIL, SB},
              BB},
             false};
        _ ->
            {Min, balance({Key, Val, max(?HEIGHT(NewS), HB) + 1, NewS, Bigger}), false}
    end.

do_remove(_Key, ?NIL) ->
    not_found;
do_remove(TargetKey, {TargetKey, _Val, _, ?NIL, ?NIL}) ->
    ?NIL;
do_remove(TargetKey, {TargetKey, _Val, _, S, ?NIL}) ->
    S;
do_remove(TargetKey, {TargetKey, _Val, _, ?NIL, B}) ->
    B;
do_remove(TargetKey, {TargetKey, _Val, _, Smaller, Bigger}) ->
    {Min, NewBigger, _} = remove_min(Bigger),
    case Min of
        ?NIL ->
            ?NIL;
        {MinK, MinV, _, _, _} ->
            balance({MinK, MinV, max(?HEIGHT(Smaller), ?HEIGHT(NewBigger)) + 1, Smaller, NewBigger})
    end;
do_remove(TargetKey, {Key, Val, _H, Smaller, Bigger}) when TargetKey < Key ->
    NewS = do_remove(TargetKey, Smaller),
    case {NewS, Bigger} of
        {not_found, _} ->
            not_found;
        {?NIL, ?NIL} ->
            {Key, Val, 1, ?NIL, ?NIL};
        {_, _} ->
            balance({Key, Val, max(?HEIGHT(NewS), ?HEIGHT(Bigger)) + 1, NewS, Bigger})
    end;
do_remove(TargetKey, {Key, Val, _H, Smaller, Bigger}) when TargetKey > Key ->
    NewB = do_remove(TargetKey, Bigger),
    case {Smaller, NewB} of
        {_, not_found} ->
            not_found;
        {?NIL, ?NIL} ->
            {Key, Val, 1, ?NIL, ?NIL};
        {_, _} ->
            balance({Key, Val, max(?HEIGHT(Smaller), ?HEIGHT(NewB)) + 1, Smaller, NewB})
    end.

remove(Key, Tree) ->
    case do_remove(Key, Tree) of
        not_found ->
            Tree;
        {_, NewTree, _} ->
            NewTree;
        NewTree ->
            NewTree
    end.

from_list(L) ->
    lists:foldl(fun({K, V}, Acc) -> insert(K, V, Acc) end, ?NIL, L).

to_list(?NIL) ->
    [];
to_list({K, V, _, S, B}) ->
    to_list(S) ++ [{K, V}] ++ to_list(B).

map(?NIL, _) ->
    [];
map({K, V, _, S, B}, F) ->
    map(S, F) ++ [F({K, V})] ++ map(B, F).

map_tree(T, F) ->
    avldict:from_list(map(T, F)).

filter(?NIL, _) ->
    [];
filter({K, V, _, S, B}, F) ->
    case F({K, V}) of
        true ->
            filter(S, F) ++ [{K, V}] ++ filter(B, F);
        false ->
            filter(S, F) ++ filter(B, F)
    end.

filter_tree(T, F) ->
    avldict:from_list(filter(T, F)).

foldl(?NIL, _, Acc) ->
    Acc;
foldl({K, V, _, S, B}, F, Acc) ->
    Acc2 = F(foldl(S, F, Acc), {K, V}),
    foldl(B, F, Acc2).

foldr(?NIL, _, Acc) ->
    Acc;
foldr({K, V, _, S, B}, F, Acc) ->
    foldr(S, F, F(foldr(B, F, Acc), {K, V})).

merge(X, Y) ->
    foldl(Y, fun(AccNode, {K, V}) -> avldict:insert(K, V, AccNode) end, X).

equal_tree(A, B) ->
    LenA = foldl(A, fun(Acc, _) -> Acc + 1 end, 0),
    LenB = foldl(B, fun(Acc, _) -> Acc + 1 end, 0),
    case LenA =:= LenB of
        false ->
            false;
        _ ->
            foldl(B, fun(Acc, {K, V}) -> Acc and (find(K, A) =:= {K, V}) end, true)
    end.
