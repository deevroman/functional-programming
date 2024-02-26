-module(input).

-export([start/1]).

start(Workers) ->
    spawn(fun() -> loop(Workers) end).

loop(Workers) ->
    case io:get_line("") of
        eof ->
            [Pid ! {stop, nil, self()} || Pid <- Workers];
        Line ->
            [Pid ! {new_point, parse_point(Line), self()} || Pid <- Workers]
    end,
    loop(Workers).

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
