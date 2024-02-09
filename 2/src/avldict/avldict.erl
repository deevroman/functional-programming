-module(avldict).
-export([
  empty_tree/0, insert/3, remove/2, find/2,
  from_list/1, to_list/1,
  merge/2, map_tree/2, filter_tree/2, foldl/3, foldr/3
]).

-define(KEY(Node), element(1, Node)).
-define(VALUE(Node), element(2, Node)).
-define(HEIGHT(Node), element(3, Node)).
-define(NIL, {nil, nil, 0, nil, nil}).

% Узел дерева представляется как кортеж вида: {Key, Value, H, S, B}
% Key — уникальный ключ
% Value — значение ключа
% H — высота узла. Считается от самого глубокого листа
% S — Левый ребёнок узла (Smaller, ибо значение должно быть меньше, чем в текущем)
% B — Правый ребёнок узла (Bigger, ибо значение должно быть больше, чем в текущем)

empty_tree() -> ?NIL.

find(_Key, ?NIL) -> not_found;
find(Key, {Key, Value, _, _, _}) -> Value;
find(Key, {Key1, _, _, Smaller, _}) when Key < Key1 -> find(Key, Smaller);
find(Key, {Key1, _, _, Bigger}) when Key > Key1 -> find(Key, Bigger).

insert(Key, Value, ?NIL) -> {Key, Value, 1, ?NIL, ?NIL};

insert(Key, Value, {K2, _V2, H2, S2, B2}) when Key == K2 ->
  {Key, Value, H2, S2, B2};

insert(Key, Value, {K2, V2, _H2, S2, B2}) when Key < K2 ->
  {K4, V4, _, S4, B4} = insert(Key, Value, S2),
  balance(S4, K4, V4, B4, K2, V2, B2);

insert(Key, Value, {K2, V2, _H2, S2, B2}) when Key > K2 ->
  {K4, V4, _, S4, B4} = insert(Key, Value, B2),
  balance(S2, K2, V2, S4, K4, V4, B4).


balance({K1, V1, H1, S1, B1}, AK, AV,
    {K2, V2, H2, S2, B2}, BK, BV,
    {K3, V3, H3, S3, B3}) when H2 > H1, H2 > H3 ->
  {K2, V2, H1 + 2,
    {AK, AV, H1 + 1, {K1, V1, H1, S1, B1}, S2},
    {BK, BV, H3 + 1, B2, {K3, V3, H3, S3, B3}}
  };

balance({K1, V1, H1, S1, B1}, AK, AV,
    {K2, V2, H2, S2, B2}, BK, BV,
    {K3, V3, H3, S3, B3}) when H1 >= H2, H1 >= H3 ->
  HB = max(H2, H3) + 1,
  HA = max(H1, HB) + 1,
  {AK, AV, HA,
    {K1, V1, H1, S1, B1},
    {BK, BV, HB, {K2, V2, H2, S2, B2}, {K3, V3, H3, S3, B3}}
  };

balance({K1, V1, H1, S1, B1}, AK, AV,
    {K2, V2, H2, S2, B2}, BK, BV,
    {K3, V3, H3, S3, B3}) when H3 >= H1, H3 >= H2 ->
  HA = max(H1, H2) + 1,
  HB = max(HA, H3) + 1,
  {BK, BV, HB,
    {AK, AV, HA, {K1, V1, H1, S1, B1}, {K2, V2, H2, S2, B2}},
    {K3, V3, H3, S3, B3}
  }.


remove_min({Key, Value, _, ?NIL, B}) -> {{Key, Value, nil, nil, nil}, B, true};
remove_min({Key, Value, _, S, ?NIL}) -> {S, {Key, Value, 1, ?NIL, ?NIL}, true};
remove_min({Key, Value, _, Smaller, Bigger}) ->
  {Min, {K, V, H, S, B}, IsLastCall} = remove_min(Smaller),
  {KB, VB, HB, SB, BB} = Bigger,
  case {IsLastCall, {K, V, H, S, B}, BB, HB} of
    {true, ?NIL, ?NIL, 2} ->
      {Min,
        {?KEY(SB), ?VALUE(SB), 2,
          {Key, Value, 1, ?NIL, ?NIL},
          {KB, VB, 1, ?NIL, ?NIL}
        }, false};
    {true, ?NIL, _, _} ->
      {Min,
        {KB, VB, max(?HEIGHT(SB) + 1, ?HEIGHT(BB)) + 1,
          {Key, Value, ?HEIGHT(SB) + 1,
            ?NIL,
            SB}, BB
        }, false};
    _ -> {Min, balance({K, V, H, S, B}, Key, Value, SB, KB, VB, BB), false}
  end.


rm(_Key, ?NIL) -> not_found;
rm(Key, {Key, _Value, _, ?NIL, ?NIL}) -> ?NIL;
rm(Key, {Key, _Value, _, S, ?NIL}) -> S;
rm(Key, {Key, _Value, _, ?NIL, B}) -> B;
rm(Key, {Key, _Value, _, Smaller, Bigger}) ->
  {Min, NewBigger, _} = remove_min(Bigger),
  case Min of
    ?NIL -> ?NIL;
    {MinK, MinV, _, _, _} ->
      {KS, VS, _, SS, BS} = Smaller,
      balance(SS, KS, VS, BS, MinK, MinV, NewBigger)
  end;
rm(Key, {Key1, Value1, _H1, Smaller, Bigger}) when Key < Key1 ->
  R = rm(Key, Smaller),
  case {R, Bigger} of
    {not_found, _} -> not_found;
    {?NIL, ?NIL} -> {Key1, Value1, 1, ?NIL, ?NIL};
    {_, {K4, V4, _, S4, B4}} -> balance(R, Key1, Value1, S4, K4, V4, B4)
  end;
rm(Key, {Key1, Value1, _H1, Smaller, Bigger}) when Key > Key1 ->
  R = rm(Key, Bigger),
  case {Smaller, R} of
    {_, not_found} -> not_found;
    {?NIL, ?NIL} -> {Key1, Value1, 1, ?NIL, ?NIL};
    {{K4, V4, _, S4, B4}, _} -> balance(S4, K4, V4, B4, Key1, Value1, R)
  end.

remove(K, T) ->
  case rm(K, T) of
    not_found -> T;
    {_, NewT, _} -> NewT;
    NewT -> NewT
  end.


from_list(L) -> lists:foldl(fun({K, V}, Acc) -> insert(K, V, Acc) end, ?NIL, L).


to_list(?NIL) -> [];
to_list({K, V, _, S, B}) ->
  to_list(S) ++ [{K, V}] ++ to_list(B).


map(?NIL, _) -> [];
map({K, V, _, S, B}, F) ->
  map(S, F) ++ [F({K, V})] ++ map(B, F).

map_tree(T, F) -> avldict:from_list(map(T, F)).


filter(?NIL, _) -> [];
filter({K, V, _, S, B}, F) ->
  case F({K, V}) of
    true -> filter(S, F) ++ [{K, V}] ++ filter(B, F);
    false -> filter(S, F) ++ filter(B, F)
  end.

filter_tree(T, F) -> avldict:from_list(filter(T, F)).


foldl(?NIL, _, Acc) -> Acc;
foldl({K, V, _, S, B}, F, Acc) ->
  Acc2 = F(foldl(S, F, Acc), {K, V}),
  foldl(B, F, Acc2).


foldr(?NIL, _, Acc) -> Acc;
foldr({K, V, _, S, B}, F, Acc) ->
  foldr(S, F, F(foldr(B, F, Acc), {K, V})).


merge(X, Y) ->
  foldl(Y, fun(AccNode, {K, V}) -> avldict:insert(K, V, AccNode) end, X).
