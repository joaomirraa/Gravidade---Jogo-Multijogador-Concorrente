-module(server).
-include("server.hrl").
-export([start/0, server/1, stop/1, userAuth/2]).

start() -> server(12345).

stop(Server) -> 
    Server ! stop,
    shutdown.

server(Port) ->
    Result = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
    case Result of
        {ok, LSock} ->
            register(offProc, spawn(fun() -> offline:start() end)),
            Pid = spawn(fun() -> init(LSock) end),
            offProc ! {load, Pid},
            Pid;
        {error, Reason} ->
            io:fwrite("Error generating socket\n"),
            Reason
    end.

init(Sock)->
    receive
        {loaded, Accs, Lvl} ->
            io:fwrite("Loaded~p~n~p\n", [Lvl, Accs]),
            register(accsProc, spawn(fun()->accounts:start(Accs, Lvl) end)),
            register(lobbyProc, spawn(fun()->lobby:start() end)),
            spawn(fun() -> acceptor(Sock) end),
            init(Sock);
        {error, Reason} ->
            io:fwrite("Error loading accounts~p\n", [Reason]),
            init(Sock);
        stop ->
            gen_tcp:close(Sock),
            accsProc ! {shutdown},
            offProc ! off
    end.

acceptor(LSock) ->
    case gen_tcp:accept(LSock) of 
        {ok, Socket} ->
            spawn(fun() -> acceptor(LSock) end),
            accsProc ! {online, self()},
            userAuth(Socket, {"Anonymous", 1, "main", 0}); %nome, nivel, sala, xp
        {error, closed} ->
            io:fwrite("Closed socket");
        {error, timeout} ->
            io:fwrite("Timeout");
        {error, _} ->
            io:fwrite("Error listening to socket")
        end.

userWait(Sock, User) ->
    %io:fwrite("User ~p waiting~n", [User]),
    receive
        {fpieces, Data} ->
            case Data of
                {new_name, Name} ->
                    {_, Level, Lobby, XP} = User,
                    userAuth(Sock, {Name, Level, Lobby, XP});
                {new_room, Room} ->
                    if Room == "main" ->
                        userAuth(Sock, User);
                    true ->
                        {UserN, Level, _, XP} = User,
                        userAuth(Sock, {UserN, Level, Room, XP})
                    end;
                {unexpected_leave} ->
                    ?SEND_MESSAGE(Sock, "res@@@unexpected_leave.\n"),
                    userAuth(Sock, User)
            end;
        {tcp, _, Data} ->
            case re:split(binary_to_list(Data), "@@@") of
                [<<?LEAVE_ROOM>>, _] ->
                    {_, _, Lobby, _} = User,
                    lobbyProc ! {offline,  Lobby, self()},
                    userAuth(Sock, User);
                _ ->
                    ?SEND_MESSAGE(Sock, "res@@@Error: Incorrect syntax.\n"),
                    userAuth(Sock, User)
            end;
        {start_game, Game, PlayerNum} ->
            game_player:start(Game, Sock, User, PlayerNum);
        {broadcast_list, Data} ->
            ?SEND_BROADCAST_LIST(Sock, Data),
            userWait(Sock, User);
        {broadcast, Data} ->
            ?SEND_BROADCAST(Sock, Data),
            userWait(Sock, User);
        {tcp_closed, _} ->
            {_, _, Lobby, _} = User,
            accsProc ! {offline, self()},
            lobbyProc ! {offline_countdown, Lobby, self()};
        {tcp_error, _, _} ->
            {_, _, Lobby, _} = User,
            accsProc ! {offline, self()},
            lobbyProc ! {offline_countdown, Lobby, self()}
    end.

userAuth(Sock, User) ->
    % io:fwrite("User ~p auth~n", [User]),
    receive
    {broadcast, Data} ->
        ?SEND_BROADCAST(Sock, Data),
        userAuth(Sock, User);
    {broadcast_list, Data} ->
        ?SEND_BROADCAST_LIST(Sock, Data),
        userAuth(Sock, User);
    {fpieces, Data} ->
        case Data of
            {unexpected_leave} ->
                userAuth(Sock, User);
            {new_name, Name} ->
                {_, Level, Lobby, XP} = User,
                userAuth(Sock, {Name, Level, Lobby, XP});
            {new_room, Room} ->
                {UserN, Level, _, XP} = User,
                userAuth(Sock, {UserN, Level, Room, XP});
            {countdown, Room} ->
                {_, _, Lobby, _} = User,
                io:fwrite("Starting countdown for ~p~n", [Room]),
                Game = spawn(fun() -> game_sim:start(Room) end),
                CountProc = spawn(fun() -> countdown(Lobby, Game) end),
                self() ! {fpieces, {wait}}, 
                lobbyProc ! {countdown_started, CountProc, Lobby},
                userAuth(Sock, User);
            {new_lvl, Lvl}->
                {UserN, _, Lobby, XP} = User,
                userAuth(Sock, {UserN, Lvl, Lobby, XP});
            {new_xp, XP}->  
                {UserN, Level, Lobby, _} = User,
                userAuth(Sock, {UserN, Level, Lobby, XP});
            {wait} ->
                userWait(Sock, User);
            _ -> 
                io:fwrite("ERROR ~p ~p\n", [self(), Data]),
                userAuth(Sock, User)
        end;
    {tcp, _, Data} ->
        case re:split(binary_to_list(Data), "@@@") of
            [<<?CREATE_ACCOUNT>>, UserName, Password] ->
                io:fwrite("creating ~p\n", [User]),
                accsProc ! {create_account, 
                string:trim(binary_to_list(UserName), trailing), 
                string:trim(binary_to_list(Password), trailing), 
                self()},
                userAuth(Sock, User);
            [<<?LOGIN_ACCOUNT>>, UserName, Password] ->
                io:format("logging in ~p\n", [User]),
                accsProc ! {login, 
                string:trim(binary_to_list(UserName), trailing), 
                string:trim(binary_to_list(Password), trailing), 
                self()},
                userAuth(Sock, User);
            [<<?LOGOUT_ACCOUNT>>, _] ->
                io:format("Logging out ~p\n", [User]),
                accsProc ! {logout, self()},
                userAuth(Sock, User);
            [<<?JOIN_ROOM>>, Room] ->
                {UserN, Level, Lobby, _} = User,
                lobbyProc ! {join, 
                    UserN, 
                    Lobby, 
                    string:trim(binary_to_list(Room), trailing), 
                    Level,
                    self()},
                userAuth(Sock, User);
            [<<?LEAVE_ROOM>>, _] ->
                {_, _, Lobby, _} = User,
                lobbyProc ! {leave, Lobby, self()},
                userAuth(Sock, User);
            [<<?CHANGE_NAME>>, Name] ->
                accsProc ! {change_name, 
                    string:trim(binary_to_list(Name), trailing), 
                self()},
                userAuth(Sock, User);
            [<<?CHANGE_PASS>>, Pass] ->
                accsProc ! {change_pass, 
                    string:trim(binary_to_list(Pass), trailing), 
                    self()},
                userAuth(Sock, User);
            [<<?REMOVE_ACCOUNT>>, _] ->
                accsProc ! {remove_account, self()},
                userAuth(Sock, User);
            [<<?CREATE_ROOM>>, Room] ->
                {UserN, Level, _, _} = User,
                lobbyProc ! {create_room, 
                    UserN,
                    string:trim(binary_to_list(Room), trailing), 
                    Level,
                    self()},
                userAuth(Sock, User);
            [<<?RANKING>>, _] ->
                accsProc ! {ranking, self()},
                userAuth(Sock, User);
            [<<?LIST_ROOMS>>, _] ->
                {_, Level, _, _} = User,
                lobbyProc ! {list_rooms, Level, self()},
                userAuth(Sock, User);
            _ ->
                ?SEND_MESSAGE(Sock, "Error: Incorrect syntax.\n"),
                userAuth(Sock, User)
        end;
    {tcp_closed, _} -> 
        {_, _, Lobby, _} = User,
        accsProc ! {offline, self()},
        lobbyProc ! {offline, Lobby, self()};
    {tcp_error, _, _} ->
        {_, _, Lobby, _} = User,
        accsProc ! {offline, self()},
        lobbyProc ! {offline, Lobby, self()};
    Other ->
        io:fwrite("ERROR ~p ~p\n", [self(), Other]),
        userAuth(Sock, User)
end.

countdown(Lobby, Game)-> 
    lobbyProc ! {countdown_started, Lobby},
    timer:sleep(5000),
    lobbyProc ! {start_game, Lobby, Game}.