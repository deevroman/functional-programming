# Функциональное программирование. Лабораторная работа №1

Вариант: 5, 26

Цель: освоить базовые приёмы и абстракции функционального программирования: функции, поток управления и поток данных,
сопоставление с образцом, рекурсия, свёртка, отображение, работа с функциями как с данными, списки.

В рамках лабораторной работы вам предлагается решить несколько задач [проекта Эйлер](https://projecteuler.net/archives).
Список задач -- ваш вариант.

Для каждой проблемы должно быть представлено несколько решений:

1. монолитные реализации с использованием:
    - хвостовой рекурсии;
    - рекурсии (вариант с хвостовой рекурсией не является примером рекурсии);
2. модульной реализации, где явно разделена генерация последовательности, фильтрация и свёртка (должны использоваться
   функции reduce/fold, filter и аналогичные);
3. генерация последовательности при помощи отображения (map);
4. работа со спец. синтаксисом для циклов (где применимо);
5. работа с бесконечными списками для языков, поддерживающих ленивые коллекции или итераторы как часть языка (к примеру
   Haskell, Clojure);
6. реализация на любом удобном для вас традиционном языке программирования для сравнения.

Требуется использовать идиоматичный для технологии стиль программирования.

Содержание отчёта:

- титульный лист;
- описание проблемы;
- ключевые элементы реализации с минимальными комментариями;
- выводы (отзыв об использованных приёмах программирования).

Примечания:

- необходимо понимание разницы между ленивыми коллекциями и итераторами;
- нужно знать особенности используемой технологии и того, как работают использованные вами приёмы.

## Задача 5. Smallest Multiple

<p>2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.</p>
<p>What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?</p>

https://projecteuler.net/problem=5

### Реализация на Python

```python
def gcd(a, b):
    if a == 0 or b == 0:
        return a + b
    return gcd(b, a % b)

def lcm(a, b):
    return a // gcd(a, b) * b
    
from functools import reduce
print(reduce(lcm, list(range(1, 20))))
```

Результат:

```232792560```

### Реализация рекурсией

```erlang
gcd(0, Y) -> Y;
gcd(X, 0) -> X;
gcd(X, Y) when X > Y -> gcd(Y, X);
gcd(X, Y) -> gcd(Y rem X, X).

lcm_rec(1) -> 1;
lcm_rec(N) -> N div gcd(N, lcm_rec(N - 1)) * lcm_rec(N - 1).

smallest_divisible_rec() -> lcm_rec(20).
```

### Реализация с хвостовой рекурсией

```erlang
gcd(0, Y) -> Y;
gcd(X, 0) -> X;
gcd(X, Y) when X > Y -> gcd(Y, X);
gcd(X, Y) -> gcd(Y rem X, X).

lcm(1, Acc) -> Acc;
lcm(N, Acc) -> lcm(N - 1, (Acc * N) div gcd(Acc, N)).

smallest_divisible_tail_rec(N) -> lcm(N, 1).

smallest_divisible_tail_rec() -> lcm(20, 1).
```

### Реализация со свёрткой

```erlang
gcd(0, Y) -> Y;
gcd(X, 0) -> X;
gcd(X, Y) when X > Y -> gcd(Y, X);
gcd(X, Y) -> gcd(Y rem X, X).

lcm(1, Acc) -> Acc;
lcm(N, Acc) -> lcm(N - 1, (Acc * N) div gcd(Acc, N)).

smallest_divisible_reduce() ->
  lists:foldl(fun(Acc, Elem) -> Acc * Elem div gcd(Acc, Elem) end, 1, lists:seq(1, 21)).

```

### infinite like via concurrency map

```erlang
gcd(0, Y) -> Y;
gcd(X, 0) -> X;
gcd(X, Y) when X > Y -> gcd(Y, X);
gcd(X, Y) -> gcd(Y rem X, X).

lcm(1, Acc) -> Acc;
lcm(N, Acc) -> lcm(N - 1, (Acc * N) div gcd(Acc, N)).


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

```

### infinite like via concurrency fold

```erlang
gcd(0, Y) -> Y;
gcd(X, 0) -> X;
gcd(X, Y) when X > Y -> gcd(Y, X);
gcd(X, Y) -> gcd(Y rem X, X).

lcm(1, Acc) -> Acc;
lcm(N, Acc) -> lcm(N - 1, (Acc * N) div gcd(Acc, N)).



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
```

### infinite like via lazy lists

```erlang
gcd(0, Y) -> Y;
gcd(X, 0) -> X;
gcd(X, Y) when X > Y -> gcd(Y, X);
gcd(X, Y) -> gcd(Y rem X, X).

lcm(1, Acc) -> Acc;
lcm(N, Acc) -> lcm(N - 1, (Acc * N) div gcd(Acc, N)).
lcm_two(A, B) -> A div gcd(A, B) * B.


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
```

## Задача 26. Reciprocal Cycles

A unit fraction contains in the numerator. The decimal representation of the unit fractions with denominators to are
given:

A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10
are given:

```
1/2 = 0.5
1/3 = 0.(3)
1/4 = 0.25
1/5 = 0.2
1/6 = 0.1(6)
1/7 = 0.(142857)
1/8 = 0.125
1/9 = 0.(1)
1/10 = 0.1
```

Where 0.1(6) means 0.166666, and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

https://projecteuler.net/problem=26

### Реализация на Python

```python
def get_period(x):
    while x % 2 == 0:
        x //= 2
    while x % 5 == 0:
        x //= 5
    if x == 1:
        return 0
    divider, power = 10, 1
    while True:
        if divider % x == 1:
            return power
        divider *= 10
        power += 1

print(max(enumerate(map(get_period, list(range(1, 1000)))), key=lambda x: x[1])[0])
```

Результат:

```982```

### Реализация рекурсией

```erlang
rec_div_five(N) when N rem 5 == 0 -> rec_div_five(N div 5);
rec_div_five(N) -> N.

rec_div_two(N) when N rem 2 == 0 -> rec_div_two(N div 2);
rec_div_two(N) -> N.

div_five_and_two(N) -> rec_div_five(rec_div_two(N)).

try_period_len(N, _, _) when N =< 1 -> 0;
try_period_len(N, Divider, Power) when Divider rem N == 1 -> Power;
try_period_len(N, Divider, Power) -> try_period_len(N, Divider * 10, Power + 1).

get_period(0) -> 0;

get_period(X) -> try_period_len(div_five_and_two(X), 10, 1).


%%% №1
get_max_period_rec(Index, _) when Index == 1 -> {1, 0};

get_max_period_rec(Index, Acc) when Index > 1 ->
  {Value, I} = get_max_period_rec(Index - 1, Acc),
  pair_max(Value, I, get_period(Index), Index).

find_number_with_max_period_rec(N) ->
  element(2, get_max_period_rec(N, {1, 0})).

find_number_with_max_period_rec() ->
  find_number_with_max_period_rec(1000).
```

### Реализация с fold

```erlang
rec_div_five(N) when N rem 5 == 0 -> rec_div_five(N div 5);
rec_div_five(N) -> N.

rec_div_two(N) when N rem 2 == 0 -> rec_div_two(N div 2);
rec_div_two(N) -> N.

div_five_and_two(N) -> rec_div_five(rec_div_two(N)).

try_period_len(N, _, _) when N =< 1 -> 0;
try_period_len(N, Divider, Power) when Divider rem N == 1 -> Power;
try_period_len(N, Divider, Power) -> try_period_len(N, Divider * 10, Power + 1).

get_period(0) -> 0;

get_period(X) -> try_period_len(div_five_and_two(X), 10, 1).


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

find_number_with_max_period_fold() ->
  find_number_with_max_period_fold(1000).
```

### Реализация с map

```erlang
rec_div_five(N) when N rem 5 == 0 -> rec_div_five(N div 5);
rec_div_five(N) -> N.

rec_div_two(N) when N rem 2 == 0 -> rec_div_two(N div 2);
rec_div_two(N) -> N.

div_five_and_two(N) -> rec_div_five(rec_div_two(N)).

try_period_len(N, _, _) when N =< 1 -> 0;
try_period_len(N, Divider, Power) when Divider rem N == 1 -> Power;
try_period_len(N, Divider, Power) -> try_period_len(N, Divider * 10, Power + 1).

get_period(0) -> 0;

get_period(X) -> try_period_len(div_five_and_two(X), 10, 1).


%%% №3
find_number_with_max_period_map(N) ->
  Arr = lists:map(fun(X) -> get_period(X) end, lists:seq(1, N)),
  string:str(Arr, [lists:max(Arr)]).

find_number_with_max_period_map() ->
  find_number_with_max_period_map(1000).
```

## Выводы

В ходе выполнения работы я познакомился с языком Erlang, его средствами разработки и базовыми принципами
функционального программирования.
Реализовал в нескольких разных стилях две задачи из проекта Эйлер. 

[К другим лабам](/)