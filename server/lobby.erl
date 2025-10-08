-module(lobby).
-include("server.hrl").
-export[start/0].

start() -> lobby(#{}).
% gestor de salas do jogo
lobby(Rooms) -> % only logged can create room CHANGE
    %% io:format("Rooms ~p~n", [Rooms]),
    receive
        {countdown_started, CountProc, Room}  ->
            case maps:is_key(Room, Rooms) of
                true -> 
                    {_, Lvl, Pids} = maps:get(Room, Rooms),
                    NRooms = maps:put(Room, {CountProc, Lvl, Pids}, Rooms),
                    io:format("Sending countdown to room ~p~n", [Room]),
                    lists:foreach(fun(Pid) -> ?SEND_MESSAGE(Pid, "res@@@countdown_started\n") end, Pids),
                    lobby(NRooms);
                false -> 
                    continue
                end, 
            lobby(Rooms);
        {start_game, Room, Game}  ->
                case maps:is_key(Room, Rooms) of
                    true ->
                        {CountProc, _, Pids} = maps:get(Room, Rooms),
                        PlayerNum = length(Pids),
                        if CountProc == self() ->
                            continue;
                        true ->
                            lists:foreach(fun(Pid) -> Pid ! {start_game, Game, PlayerNum}, ?SEND_MESSAGE(Pid, "res@@@enter_game\n") end, Pids)
                        end,
                        lobby(Rooms);
                    false ->
                        lobby(Rooms)
                end;
        {join, User, Lobby, Room, Level, Pid} -> % jogador tenta entrar numa sala
            if Lobby == "main" ->
                if User == "Anonymous" ->
                    ?SEND_MESSAGE(Pid, "res@@@Precisas de fazer login\n"),
                    lobby(Rooms);
                true ->
                    case maps:is_key(Room, Rooms) of
                        true ->
                            {CountProc, RLevel, Pids} = maps:get(Room, Rooms),
                            if (Level == RLevel orelse Level == RLevel + 1 orelse Level == RLevel - 1) andalso (CountProc == self()) -> % nivel de dificuldade e nao comecou
                                if length(Pids) < 4 -> % maximo de jogadores
                                    NRooms = maps:put(Room, {CountProc, RLevel, [Pid | Pids]} , Rooms),
                                    ?CHANGE_STATE(Pid, {new_room, Room}),
                                    io:fwrite("User ~p joined room ~p~n", [User, Room]),
                                    ?SEND_MESSAGE(Pid, "res@@@success\n"),
                                    if (length(Pids) + 1) == 2 -> %% start countdown
                                        ?CHANGE_STATE(Pid, {countdown, Room}); 
                                    true ->
                                        ?CHANGE_STATE(Pid, {wait}) % espera para comecar o jogo
                                    end,
                                    lobby(NRooms);
                                true ->
                                    ?SEND_MESSAGE(Pid, "res@@@Sala cheia\n"),
                                    lobby(Rooms)
                                end;
                            true ->
                                ?SEND_MESSAGE(Pid, "res@@@Nivel diferente da sala\n"),
                                lobby(Rooms)
                            end;
                        false ->
                            ?SEND_MESSAGE(Pid, "res@@@Sala nao existe\n"),
                            lobby(Rooms)
                    end
                end;
            true ->
                ?SEND_MESSAGE(Pid, "res@@@Ja estas noutra sala, sai primeiro\n"),
                lobby(Rooms)
            end;
        {create_room, User, Room, Level, Pid} -> % jogador tenta criar sala
            if User == "Anonymous" ->
                ?SEND_MESSAGE(Pid, "res@@@Precisas de fazer login\n"),
                lobby(Rooms);
            true ->
                case maps:is_key(Room, Rooms) of
                    true ->
                        ?SEND_MESSAGE(Pid, "res@@@Esta sala ja existe\n"),
                        lobby(Rooms);
                    false ->
                        ?SEND_MESSAGE(Pid, "res@@@success\n"),
                        NRooms = maps:put(Room, {self(), Level, []}, Rooms),
                        lobby(NRooms)
                end
            end;
        {list_rooms, Level, Pid} -> % lista as salas ao jogador -- resolver
            Ver = fun(Key, Value, Acc) -> 
                {CountProc, RLevel, Pids} = Value,
                if (Level == RLevel orelse Level == RLevel + 1 orelse Level == RLevel - 1) andalso (length(Pids) < 4) andalso (CountProc == self()) ->
                    [Key | Acc];
                true ->
                    Acc
                end
            end,
            RoomsList = maps:fold(Ver, [], Rooms),
            case length(RoomsList) of
                0 -> ?SEND_MESSAGE(Pid, "res@@@\n");
                _ -> 
                    ?SEND_MESSAGE(Pid, "res"),
                    lists:foreach(fun(Room) -> ?SEND_MESSAGE(Pid, "@@@" ++ Room) end, RoomsList),
                    ?SEND_MESSAGE(Pid, "\n")
            end,
            lobby(Rooms);
        {leave, Room, Pid} -> % jogador tenta sair da sala, se estiver numa
            if Room == "main" ->
                ?SEND_MESSAGE(Pid, "res@@@Nao estas em nenhuma sala\n"),
                lobby(Rooms);
            true->
                case maps:is_key(Room, Rooms) of
                    true ->
                        {CountProc, Level, Pids} = maps:get(Room, Rooms),
                        if length(Pids) =< 2 ->
                            NRooms = maps:remove(Room, Rooms),
                            ?SEND_MESSAGE(Pid, "res@@@success\n"),
                            io:format("User ~p left room ~p~n", [Pid, Room]),
                            ?CHANGE_STATE(Pid, {new_room, "main"}),
                            lobby(NRooms);
                        true ->
                            NRooms = maps:put(Room, {CountProc, Level, lists:delete(Pid, Pids)}, Rooms),
                            ?SEND_MESSAGE(Pid, "res@@@success\n"),
                            ?CHANGE_STATE(Pid, {new_room, "main"}),
                            lobby(NRooms)
                        end;
                    false ->
                        lobby(Rooms)
                end
            end;
        {offline, Room, Pid} -> % jogador e eliminado das salas caso saia inesperadamente
            if Room == "main" ->
                lobby(Rooms);
            true->
                case maps:is_key(Room, Rooms) of
                    true ->
                        {CountProc, RLevel, Pids} = maps:get(Room, Rooms),
                        if CountProc == self() ->
                            self() ! {leave, Room, Pid},
                            lobby(Rooms);
                        true ->
                            if length(Pids) =< 2 -> %stop countdown/ignore i
                                NRooms = maps:put(Room, {self(), RLevel, Pids}, Rooms),
                                lists:foreach(fun(PPid) -> ?CHANGE_STATE(PPid, {unexpected_leave}), ?SEND_MESSAGE(Pid, "res@@@unexpected_leave\n") end, Pids),
                                self() ! {leave, Room, Pid},
                                lobby(NRooms);
                            true ->
                                continue
                            end
                        end;
                    false ->
                        lobby(Rooms)
                end
            end
    end.