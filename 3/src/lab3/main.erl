#!/usr/bin/env escript
-module(main).

-import(input, []).
-import(output, []).
-import(linear, []).
-import(lagrange, []).
-import(gauss, []).

-export([]).

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
                m := Methods} =
                  Args) ->
             io:format("~p~n", [Args]),
             OutputPid = output:start(),
             link(OutputPid),
             Workers =
                 lists:map(fun(Method) ->
                              case Method of
                                  linear ->
                                      Pid = linear:start(OutputPid, Step),
                                      link(Pid),
                                      Pid;
                                  lagrange ->
                                      Pid = lagrange:start(OutputPid, Step, WindowLen),
                                      link(Pid),
                                      Pid;
                                  gauss ->
                                      Pid = gauss:start(OutputPid, Step, WindowLen),
                                      link(Pid),
                                      Pid;
                                  _ ->
                                      io:format("~p~n", [Method])
                              end
                           end,
                           Methods),
             InputPid = input:start(Workers),
             wait_processes(InputPid, OutputPid, Workers)
          end}.

wait_processes(InputPid, OutputPid, Workers) ->
    case {erlang:process_info(InputPid), erlang:process_info(OutputPid)} of
        {undefined, undefined} ->
            exit;
        _ ->
            wait_processes(InputPid, OutputPid, Workers)
    end.
