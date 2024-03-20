-module(server).

-export([start/1, server/1, accept/2]).

-record(message, {unixtime = {}, nickname = {}, text = {}}).

start([Port]) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(message,
                        [{attributes, record_info(fields, message)},
                         {type, ordered_set},
                         {disc_copies, [node()]}]),
    spawn(?MODULE, server, [list_to_integer(atom_to_list(Port))]),
    loop().

loop() ->
    receive
        _ ->
            loop()
    end.

server(Port) ->
    %%    io:format("start server at port ~p~n", [Port]),
    {ok, ListenSocket} =
        gen_tcp:listen(Port, [binary, {active, false}, {packet, 2}, {reuseaddr, true}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, 5)],
    timer:sleep(infinity),
    ok.

accept(Id, ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    handle_connection(Id, ListenSocket, Socket).

handle_connection(Id, ListenSocket, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Msg} ->
            logger:info("Socket #~p got message: ~p~n", [Id, binary_to_term(Msg)]),
            Res = handle_message(Socket, binary_to_term(Msg)),
            gen_tcp:send(Socket, term_to_binary(Res)),
            handle_connection(Id, ListenSocket, Socket);
        {error, closed} ->
            logger:info("Socket #~p, session closed ~n", [Id]),
            accept(Id, ListenSocket)
    end.

handle_message(Socket, Msg) ->
    case Msg of
        {connect, _Nickname} ->
            gen_server:cast(broadcaster, {new_client, Socket}),
            nil;
        {get_chat_history} ->
            F = fun() ->
                   mnesia:select(message,
                                 [{#message{unixtime = '_',
                                            nickname = '_',
                                            text = '_'},
                                   [],
                                   ['$_']}])
                end,
            {_, Res} = mnesia:transaction(F),
            {chat_history, Res};
        {new_message,
         #message{unixtime = _,
                  nickname = _,
                  text = _} =
             Message} ->
            F = fun() -> mnesia:write(Message) end,
            mnesia:transaction(F),
            gen_server:cast(broadcaster, {new_message, Message}),
            case whereis(client) of
                undefined ->
                    ok;
                Pid ->
                    Pid ! {send, {new_message, Message}},
                    ok
            end;
        Oth ->
            io:format("Unknown event from client: ~p~n", [Oth])
    end.
