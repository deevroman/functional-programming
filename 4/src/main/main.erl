-module(main).

-export([main/1]).

main([ServerPort]) ->
    broadcaster:start_link(),
    ServerPid = spawn(server, start, [[ServerPort]]),
    link(ServerPid),
    ask_user().

ask_user() ->
    io:format("Type <host> <port> for connection~n"),
    [Host, Port] =
        string:tokens(
            string:trim(
                io:get_line("")),
            " "),
    io:format("Type your nickname: "),
    Nickname =
        Nickname =
            string:trim(
                io:get_line("")),
    ClientPid = spawn(client, start, [Host, list_to_integer(Port), Nickname]),
    link(ClientPid),
    register(client, ClientPid),
    client:sync_chat_history(ClientPid),
    io:format("!send text~n"),
    input_loop(ClientPid, Nickname).

input_loop(ClientPid, Nickname) ->
    case io:get_line("") of
        eof ->
            ok;
        Line when is_list(Line) ->
            process_line(Line, ClientPid, Nickname),
            input_loop(ClientPid, Nickname);
        Oth ->
            io:format("Unknow input error: '~p'~n", [Oth])
    end.

process_line(Line, ClientPid, Nickname) ->
    [Command | Args] =
        string:tokens(
            string:trim(Line), " "),
    case Command of
        "!send" ->
            client:send_message(ClientPid, Nickname, string:join(Args, ""));
        "!recon" ->
            exit(reconnect);
        _ ->
            io:format("Unknow command: '~s'~n", [Line])
    end.
