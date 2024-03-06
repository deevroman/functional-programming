-module(input).

-export([start/1]).

start(Workers) ->
    spawn(fun() -> loop(Workers) end).

loop(Workers) ->
    case io:get_line("") of
        eof ->
            [Worker ! {stop, nil, self()} || Worker <- Workers];
        Line ->
            [Worker ! {new_point, parse_point(Line), self()} || Worker <- Workers],
            loop(Workers)
    end.

parse_point(Line) ->
    XY = string:tokens(
             string:trim(Line), " "),
    [case string:to_float(X) of
         {error, no_float} ->
             list_to_integer(X);
         {F, _} ->
             F
     end
     || X <- XY].
