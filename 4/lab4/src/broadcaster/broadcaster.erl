-module(broadcaster).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, start_link/0]).

-record(state, {sockets = []}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, _State) ->
    erlang:error(not_implemented).

handle_cast({new_client, UserSocket}, #state{sockets = Sockets}) ->
    {noreply, #state{sockets = [UserSocket | Sockets]}};
handle_cast({new_message, _} = Event, #state{sockets = Sockets}) ->
    NewSockets =
        lists:foldl(fun(Socket, Acc) ->
                       try
                           ok = gen_tcp:send(Socket, term_to_binary(Event)),
                           [Socket | Acc]
                       catch
                           error:_Error ->
                               Acc
                       end
                    end,
                    [],
                    Sockets),
    {noreply, #state{sockets = NewSockets}}.
