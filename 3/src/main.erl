#!/usr/bin/env escript
-module(main).

-import(input, []).
-import(output, []).
-import(linear, []).
-import(lagrange, []).
-import(gauss, []).

-export([main/0]).
-export([main/1]).

main() ->
    main(["-m", "lagrange"]).

main(Args) ->
    argparse:run(Args, cli(), #{progname => lab3}).

cli() ->
    #{arguments =>
          [#{name => s,
             default => 0.2,
             type => float,
             short => $s,
             help => "Step"},
           #{name => w,
             default => 5,
             type => float,
             short => $w,
             help => "Window lenght"},
           #{name => m,
             short => $m,
             action => append,
             type => {atom, [linear, lagrange, gauss]}}],
      handler =>
          fun(#{s := Step,
                w := WindowLen,
                m := Methods}) ->
             OutputPid = output:start(),
             Workers =
                 lists:map(fun(Method) ->
                              case Method of
                                  linear ->
                                      linear:start(OutputPid, Step);
                                  lagrange ->
                                      lagrange:start(OutputPid, Step, WindowLen);
                                  gauss ->
                                      gauss:start(OutputPid, Step, WindowLen);
                                  _ ->
                                      io:format("Unknown method: ~p~n", [Method])
                              end
                           end,
                           Methods),
             InputPid = input:start(Workers),
             wait_processes(InputPid, OutputPid, Workers)
          end}.

wait_processes(InputPid, OutputPid, Workers) ->
    case {erlang:process_info(InputPid),
          erlang:process_info(OutputPid),
          lists:all(fun(W) -> erlang:process_info(W) == undefined end, Workers)}
    of
        {undefined, undefined, true} ->
            io:format("End.", []),
            exit;
        {undefined, _, true} ->
            OutputPid ! {stop, nil, self()},
            wait_processes(InputPid, OutputPid, Workers);
        _ ->
            wait_processes(InputPid, OutputPid, Workers)
    end.
