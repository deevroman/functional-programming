-module(client).

-export([start/3, send/2, stop/1, sync_chat_history/1, send_message/3]).

-record(message, {unixtime = {}, nickname = {}, text = {}}).

start(Host, Port, Nickname) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(message,
                        [{attributes, record_info(fields, message)},
                         {type, ordered_set},
                         {disc_copies, [node()]}]),

    logger:info("Client ~p connects to ~s:~p~n", [self(), Host, Port]),
    case gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, 2}]) of
        {ok, Socket} ->
            connect(self(), Nickname),
            loop(Socket, Nickname);
        {error, _} ->
            io:format("There is no connection to the server. Type <host> <port> again:~n", [])
    end.

connect(Pid, Nickname) ->
    send(Pid, {connect, Nickname}).

sync_chat_history(Pid) ->
    send(Pid, {get_chat_history}).

send_message(Pid, Nickname, Text) ->
    Time = os:timestamp(),
    Message =
        #message{unixtime = Time,
                 nickname = Nickname,
                 text = Text},
    F = fun() -> mnesia:write(Message) end,
    mnesia:transaction(F),
    send(Pid, {new_message, Message}),
    gen_server:cast(broadcaster, {new_message, Message}),

    {_, Messages} =
        mnesia:transaction(fun() ->
                              mnesia:select(message,
                                            [{#message{unixtime = '_',
                                                       nickname = '_',
                                                       text = '_'},
                                              [],
                                              ['$_']}])
                           end),
    print_messages(Messages).

send(Pid, Msg) ->
    Pid ! {send, Msg},
    ok.

stop(Pid) ->
    Pid ! stop,
    ok.

loop(Socket, Nickname) ->
    receive
        {send, Msg} ->
            logger:info("Client ~p send ~p~n", [self(), Msg]),
            gen_tcp:send(Socket, term_to_binary(Msg)),
            loop(Socket, Nickname);
        {tcp, Socket, Event} ->
            handle_event(binary_to_term(Event), Nickname),
            loop(Socket, Nickname);
        stop ->
            logger:info("Client ~p closes connection and stops~n", [self()]),
            gen_tcp:close(Socket)
    after 200 ->
        loop(Socket, Nickname)
    end.

new_message_handler(#message{text = Text,
                             nickname = MsgNickname,
                             unixtime = DateTime} =
                        Msg,
                    ClientNickname) ->
    print_message(DateTime, MsgNickname, Text),
    case MsgNickname of
        ClientNickname ->
            nil;
        _ ->
            {_, Res} = mnesia:transaction(fun() -> mnesia:select(message, [{Msg, [], ['$_']}]) end),
            case Res of
                [_] ->
                    ok;
                _ ->
                    mnesia:transaction(fun() -> mnesia:write(Msg) end),
                    gen_server:cast(broadcaster, {new_message, Msg}),
                    ok
            end
    end.

handle_event(Event, ClientNickname) ->
    case Event of
        {new_message, Msg} ->
            new_message_handler(Msg, ClientNickname);
        {chat_history, Messages} ->
            print_messages(Messages);
        nil ->
            ok;
        ok ->
            ok;
        Event ->
            io:format("Unknown event: ~p~n", [Event])
    end.

print_messages(Messages) ->
    io:format(
        os:cmd(clear)),
    do_print_messages(Messages).

do_print_messages([]) ->
    io:format("", []);
do_print_messages([#message{text = Text,
                            nickname = Nickname,
                            unixtime = DateTime}
                   | T]) ->
    print_message(DateTime, Nickname, Text),
    do_print_messages(T).

print_message(DateTime, Nickname, Text) ->
    {_Date, {Hours, Minutes, Seconds}} = calendar:now_to_datetime(DateTime),
    io:format("~2..0B:~2..0B:~2..0B \e[32m<~s>:\e[0m ~s~n",
              [Hours, Minutes, Seconds, Nickname, Text]).
