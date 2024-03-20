-module(lab4_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 10,
          period => 10},
    ChildSpecs =
        [#{id => broadcaster,
           start => {broadcaster, start_link, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [broadcaster]},
         #{id => server,
           start => {server, start, [[list_to_atom(os:getenv("PORT"))]]},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [server]},
         #{id => main,
           start => {main, ask_user, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [main]}],
    {ok, {SupFlags, ChildSpecs}}.
