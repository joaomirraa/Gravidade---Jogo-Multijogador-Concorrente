-module(game_sim).
-include("server.hrl").
-export([start/1]).
-define(TICK_RATE, 500). % Testing with 1 second ticks - change later and see what works best
-include("pvectors.hrl").
-define(PLANET_RADIUS, 50). % Radius of the planets

start(Name) -> 
    % cria um ticker
    %% io:fwrite("Game ~p started~n", [Name]),
    GameProc = self(),
    register(planets_manager, spawn(fun() -> planets_manager(GameProc, maps:new()) end)),
    register(collision_manager, spawn(fun() -> check_pairs(GameProc) end)),
    Chat = spawn(fun() -> gameChat(GameProc, maps:new()) end),
    gameSim(Chat, Name, maps:new(), false , 0, maps:new()). 

gameTick(GameProc) ->
    receive
        stop ->
            ok
        after  % Proceed anyway after TICK_RATE milliseconds
            ?TICK_RATE ->
                GameProc ! {tick},
                gameTick(GameProc)
    end.


gameChat(GameProc, OnChat) ->
    receive
        {new_message, Pid, Message} -> % sends message to all players
            ToSend = "chat@@@" ++ maps:get(Pid, OnChat) ++ "@@@sent:@@@" ++ Message ++ "\n",
            lists:foreach(fun(Key) -> ?SEND_MESSAGE(Key, ToSend) end, maps:keys(OnChat)),
            gameChat(GameProc, OnChat);
        {pid_left, Pid} -> % removes a pid from the chat
            Username = maps:get(Pid, OnChat),
            ToSend = "chat@@@" ++ Username ++ "@@@left chat\n",
            NewOnChat = maps:remove(Pid, OnChat),
            lists:foreach(fun(Key) -> ?SEND_MESSAGE(Key, ToSend) end, NewOnChat),
            gameChat(GameProc, NewOnChat);
        {new_pid, Pid, Username} -> % adds a new pid to the chat
            gameChat(GameProc, maps:put(Pid, Username, OnChat))
    end.

afterGame(Chat, Name, Pids) ->
    receive
        {send_message, Pid, Message} -> % sends message to all players
            Chat ! {new_message, Pid, Message},
            afterGame(Chat, Name, Pids);
        {leave_chat, Pid} ->
            Chat ! {pid_left, Pid},
            ?SEND_MESSAGE(Pid, "end_game\n"),
            ?CHANGE_STATE(Pid, {end_game}),
            lobbyProc ! {leave, Name, Pid}
    end.

gameSim(Chat, Name, Pids, Countdown, PlayerCount, Indexes) -> % pids => {alive?, username,playerstate} IMPLEMENTAR COUNTDOWN
    %%io:format("Game ~p\n", [Pids]),
    receive
        {tick} -> % request and sends info of the current to all players
            io:fwrite("Got the tick\n"),
            lists:foreach(fun(Pid) ->
                case maps:get(Pid, Pids) of
                    {true, _, _} -> 
                        Pid ! {player_state};
                    _ ->
                        continue
                end
            end, maps:keys(Pids)),
            %io:format("ticked ~p\n", [Pids]),
            gameSim(Chat, Name, Pids, Countdown, PlayerCount, Indexes);
        {player_state, Pid, PlayerState, Index} -> % get player state
            {Alive, Username, _} = maps:get(Pid, Pids),
            NewPids = maps:put(Pid, {Alive,Username,PlayerState}, Pids),
            Indexes1 = maps:put(Pid, {Index, true}, Indexes),
            %io:format("Player ~p state ~p\n", [Username, PlayerState]), 
            Bool = lists:all(fun({_, Value}) -> Value == true end, maps:values(Indexes1)),
            if Bool ->   % if all are marked as true <received>, check collisions and stuff
                AlivePredicate = fun(_Pid, {Living, _, _}) -> Living == true end,
                AlivePidStates = maps:filter(AlivePredicate, NewPids),
                collision_manager ! {check_pairs, AlivePidStates},
                planets_manager ! {tick, AlivePidStates}, % send the new states to the planets manager
                Indexes2 = maps:fold(fun(_Pid, {_Index,_}, Acc) -> 
                        maps:put(_Pid, {_Index, false}, Acc)
                    end, Indexes1, Indexes1),
                receive 
                    {update, NewPidStates} ->
                        % Update states and send them to all players - collision manager only checked for alive players
                        UpdatePids = maps:fold(fun(Key, Value, Acc) -> 
                            maps:put(Key, Value, Acc)
                        end, NewPids, NewPidStates),
                        send_states(UpdatePids,Indexes2),  % Send the updated states to all players if there was a change
                        gameSim(Chat, Name, UpdatePids, Countdown, PlayerCount, Indexes2);
                    {ok} ->
                        send_states(NewPids, Indexes),       % Send the updated states to all players 
                        gameSim(Chat, Name, NewPids, Countdown, PlayerCount, Indexes2) % No changes, continue
                end;
            true ->
                gameSim(Chat, Name, NewPids, Countdown, PlayerCount, Indexes1)
            end;
        {starting_pos, Pid, StartingState,Index} -> % get player state
            {Alive, Username,_} = maps:get(Pid, Pids),
            NewPids = maps:put(Pid, {Alive, Username, StartingState}, Pids),
            NewIndexes = maps:put(Pid, {Index, false}, Indexes), 
            IndexLength = length(maps:keys(NewIndexes)),
            PidsLength = length(maps:keys(NewPids)),
            case IndexLength == PidsLength of
                true -> % all players have started
                    io:format("All players have started ~p\n", [maps:keys(NewIndexes)]),
                    lists:foreach(fun(Key) -> % sends to all players that all players have started
                        {_, Username, {_, Boost, {Pos, _, _, Angle}, _}} = maps:get(Key, NewPids),
                        {Index1, _} = maps:get(Key, NewIndexes),
                        RelevantData = [Index1, Username, Boost, Pos#pvector.x, Pos#pvector.y, Angle],
                        lists:foreach(fun(Pid_) ->
                            FormattedData = io_lib:format("pos~w@@@~w@@@~w@@@~w@@@~w@@@~w\n", RelevantData),
                            StringData = lists:flatten(FormattedData),
                            ?SEND_MESSAGE(Pid_, StringData)
                        end, maps:keys(NewIndexes))
                    end, maps:keys(NewIndexes)),
                    gameSim(Chat, Name, NewPids, Countdown, PidsLength, NewIndexes);
                false -> % not all players have started
                    gameSim(Chat, Name, NewPids, Countdown, PlayerCount, NewIndexes)
            end;
        {countdown_started} ->
            lists:foreach(fun(Key) -> % sends to all players that last player countdown started
                ?SEND_MESSAGE(Key, "game@@@countdown_start\n")
            end, maps:keys(Pids)),
            gameSim(Chat, Name, Pids, true, PlayerCount, Indexes);
        {countdown_ended} ->
            lists:foreach(fun(Key) -> % sends to all players that last player countdown ended
                ?SEND_MESSAGE(Key, "game@@@countdown_end\n")
            end, maps:keys(Pids)),
            gameSim(Chat, Name, Pids, false, PlayerCount, Indexes);
        {new_pid, Username, Pid, PlayerNum} -> % add a new pid to the game     
            NewPids = maps:put(Pid, {true, Username, #{}}, Pids),
            Count = maps:size(NewPids),
            Pid ! {start_pos, Count},
            %io:format("Index is currently ~p\n", [Index]),
            if Count == PlayerNum ->
                io:format("All players joined\n"),
                self() ! {loading},
                gameSim(Chat, Name, NewPids, Countdown, PlayerNum, Indexes);
            true ->
                gameSim(Chat, Name,NewPids, Countdown, PlayerNum, Indexes)
            end;
        {loading} -> 
            %% safety loading to make sure all players threads are running and gameStates have been updated
            %% also the loading screen should be displayed because it took effort to make it
            planets_manager ! {launch_planets,Indexes},
            io:format("Loading\n"),
            timer:sleep(3000),
            GameProcMe = self(),
            spawn(fun() -> gameTick(GameProcMe) end),
            gameSim(Chat, Name, Pids, Countdown, PlayerCount, Indexes);
        {send_message, Pid, Message} -> % sends message to all players
            Chat ! {new_message, Pid, Message},
            gameSim(Chat, Name, Pids, Countdown, PlayerCount, Indexes);
        {died, Pid} ->
            NewIndexes = maps:remove(Pid, Indexes),
            {_, Username,_} = maps:get(Pid, Pids),
            io:format("Player ~p died by crashing into the fucking sun\n", [Username]),
            ?CHANGE_STATE(Pid, {died}),
            Chat ! {new_pid, Pid, Username},
            Chat ! {new_message, Pid, Username ++ "died\n"},
            NewAlives = lists:foldl( % gets the alive pids and 
                fun(Key, AccAlives) ->
                    case maps:get(Key, Pids) of
                        {Alive, _, _} when Alive == true ->
                            [Key | AccAlives];
                        _ ->
                            AccAlives
                    end
                end, [], maps:keys(Pids)),
            {_, Username, _} = maps:get(Pid, Pids),
            lists:foreach(fun(Key) -> % sends to all players the pid that died
                String = Username ++ "@@@died\n",
                ?SEND_MESSAGE(Key, String)
            end, maps:keys(Pids)),
            NewPids = maps:put(Pid, {false, Username, #{}}, Pids),
            case length(NewAlives) == 2 of
                true -> % start countdown on another proccess
                    GameProc = self(),
                    spawn(fun() -> countdown(GameProc) end),
                    gameSim(Chat, Name, NewPids, true, PlayerCount, NewIndexes);
                false ->
                    case length(NewAlives) == 1 of
                        true ->
                            case NewAlives of
                                [LastAlive] -> % last alive wins
                                    self() ! {start_end_game, LastAlive},
                                    gameSim(Chat, Name, NewPids, Countdown, PlayerCount, NewIndexes)
                            end;
                        false ->
                            gameSim(Chat, Name, NewPids, Countdown, PlayerCount, NewIndexes)
                    end
            end;
        {start_end_game, LastAlive} -> 
            if Countdown -> % countdown still active - todos perdem, send lost to all por causa do xp para por a 0
                lists:foreach(fun(Pid) -> 
                    ?SEND_MESSAGE(Pid, "lost_game\n"),
                    ?CHANGE_STATE(Pid, {lost})
                end, maps:keys(Pids)),
                afterGame(Chat, Name, Pids);
            true -> % todos menos lastalive perdem
                lists:foreach(fun(Pid) -> 
                    if Pid == LastAlive -> 
                        ?CHANGE_STATE(Pid, {won}),
                        ?SEND_MESSAGE(Pid, "won_game\n");
                    true ->
                        ?CHANGE_STATE(Pid, {lost}),
                        ?SEND_MESSAGE(Pid, "lost_game\n")
                    end
                end, maps:keys(Pids)),
                self() ! {end_game},
                afterGame(Chat, Name, Pids)
            end;
        {interrupt_game, RIP} -> % ends the game removing all pids
            NewPids = maps:remove(RIP, Pids),
            lists:foreach(fun(Pid) -> 
                ?SEND_MESSAGE(Pid, "interrupt_game\n"),
                lobbyProc ! {offline, Name, Pid}
            end, maps:keys(NewPids));
        Data ->
            io:format("Unexpected data ~p\n", [Data]),
            gameSim(Chat, Name, Pids, Countdown, PlayerCount, Indexes)
    end.

countdown(GameProc)-> 
    io:format("Countdown started ~p\n", [GameProc]),
    GameProc !  {countdown_started},
    timer:sleep(5000),
    io:format("Countdown ended\n"),
    GameProc !  {countdown_ended}.



send_states(States,Indexes) ->
    LoopStates = maps:keys(States),

    lists:foreach(fun(Key) -> 
        State = {Alive,_,_} = maps:get(Key, States),
        case Alive of
            true ->                        
                {_,Username,{_,Boost,{Pos,_,_,Angle},_}} = State,
                {Index,_} = maps:get(Key, Indexes),
                RelevantData = [Index,Username,Boost,Pos#pvector.x,Pos#pvector.y,Angle],
                FormattedData = io_lib:format("pos~w@@@~w@@@~w@@@~w@@@~w@@@~w\n", RelevantData),
                StringData = lists:flatten(FormattedData),
                lists:foreach(fun(Key2) ->
                    ?SEND_MESSAGE(Key2, StringData)
                end, LoopStates);
            _ ->
                ok
            end
    end, LoopStates).


        
check_pairs(CallerPid) ->
    % {Alive , Username,                    PlayerState                                    ,KeyMap}
    % {Alive, Username, {UserAuth, 100, {{X,Y}, {VelX,VelY}, {AccelX,AccelY}, Angle}}, KeyMap}
    receive 
        {check_pairs, PidStates} -> 
            % If there are less than 2 players left, there's no need for collision checks
            case length(maps:keys(PidStates)) of
                0 -> 
                    CallerPid ! {ok},
                    check_pairs(CallerPid);
                1 -> 
                    CallerPid ! {ok},
                    check_pairs(CallerPid);
                _ -> 
                    Pairs = generate_pairs(maps:keys(PidStates), []), % Generate pairs of Pids
                    UpdatedPids = lists:foldl(
                        fun({Pid1, Pid2}, Acc) ->
                            State1 = maps:get(Pid1, PidStates),
                            State2 = maps:get(Pid2, PidStates),
                            {P1, P2, S1, S2} = check_collisions(Pid1, Pid2, State1, State2),
                            Acc1 = maps:put(P1, S1, Acc),
                            Acc2 = maps:put(P2, S2, Acc1),
                            Acc2
                        end,
                        PidStates,
                        Pairs
                    ),
                    CallerPid ! {update, UpdatedPids},
                    check_pairs(CallerPid)
                end;
        {stop} ->
            ok
    end.



check_collisions(Pid1, Pid2, State1, State2) ->
    {Pid1,Pid2,State1,State2}.

generate_pairs([], Acc) -> Acc;
generate_pairs([Pid | Rest], Acc) ->
    NewPairs = [{Pid, OtherPid} || OtherPid <- Rest],
    generate_pairs(Rest, Acc ++ NewPairs).



planets_manager(GameProc, PlanetStates) ->
    receive
        {launch_planets, Indexes} ->
            RandInt = rand:uniform(2), % 1 to 2
            PlanetCount = 2 + RandInt, % 3 to 4 planets in total
            StartPlanetStates = launch_planets(PlanetCount,PlanetStates),
            io:format("Indexes Keys ~p\n", [maps:keys(Indexes)]),
            lists:foreach(fun(Key) ->
                lists:foreach(fun(PlanetKey) ->
                    {Pos,Vel} = maps:get(PlanetKey, StartPlanetStates),
                    FormattedData = io_lib:format("p~w@@@~w@@@~w@@@~w@@@~w\n", [PlanetKey,Pos#pvector.x,Pos#pvector.y,Vel#pvector.x,Vel#pvector.y]),
                    StringData = lists:flatten(FormattedData),
                    ?SEND_MESSAGE(Key, StringData)
                end, maps:keys(StartPlanetStates))
            end, maps:keys(Indexes)),
            % Send planet states to all players - confirm that the client can't simply emulate this from here on
            planets_manager(GameProc, StartPlanetStates);
        {tick, PlayerStates} ->
            lists:foreach(fun(Key) ->
                {_,_,{_,_,{Pos,_,_,_},_}} = maps:get(Key, PlayerStates),
                Died = planet_collision(Pos, PlanetStates),
                case Died of
                    true ->
                        GameProc ! {died, Key};
                    _ ->
                        ok
                end
            end, maps:keys(PlayerStates)),
            % Check for collisions between players and planets
            % Inform gameProc that a player died if applicable
            self () ! {pre_calc, PlanetStates},
            planets_manager(GameProc, PlanetStates);
        {pre_calc, PlanetStates} ->
            % Pre-calculate the next planet states for the next frame 
            NextPlanetStates = getNextPlanetStates(PlanetStates),
            planets_manager(GameProc, NextPlanetStates)
    end.


planet_collision(PlayerPos, PlanetStates) ->
    lists:any(fun({Pos, _}) -> 
        Distance = pvector_dist(PlayerPos, Pos),
        Distance < ?PLANET_RADIUS + 20  %% Planet radius + (player radius-5) -5 is for helping the player
    end, maps:values(PlanetStates)).

launch_planets(0, PlanetStates) -> 
    PlanetStates;
launch_planets(Count,PlanetStates) ->    
    SunPos = #pvector{x = 1980/2,y=1080/2},
    XorY = rand:uniform(),   % True for x, false for y  -> variable that decides if the planet will be on the x or y axis
    if XorY > 0.5 ->     % Planet will be positioned horizontally
        LeftOrRight = rand:uniform(), % True for left, false for right
        if LeftOrRight > 0.5 -> % Planet will be positioned on the left
            X = random_between(-300, SunPos#pvector.x - 300),
            Location = #pvector{x=X,y=SunPos#pvector.y},
            VY = random_between(-4, -9),
            Vel = #pvector{x=0,y=VY};
        true -> % Planet will be positioned on the right
            X = random_between(SunPos#pvector.x + 300, 1920 + 300),
            Location = #pvector{x=X,y=SunPos#pvector.y},
            VY = random_between(-4, -9),
            Vel = #pvector{x=0,y=VY}
        end;
    true -> % Planet will be positioned vertically
        UpOrDown = rand:uniform(), % True for up, false for down
        if UpOrDown > 0.5 -> % Planet will be positioned on the top
            Y = random_between(-300, SunPos#pvector.y - 300),
            Location = #pvector{x=1920/2,y=Y},
            VX = random_between(-4, -9),
            Vel = #pvector{x=VX,y=0};
        true -> % Planet will be positioned on the bottom
            Y = random_between(SunPos#pvector.y + 300, 1080 + 300),
            Location = #pvector{x=1920/2,y=Y},
            VX = random_between(-4, -9),
            Vel = #pvector{x=VX,y=0}            
        end
    end,
    NewPlanetStates = maps:put(Count, {Location, Vel}, PlanetStates),
    launch_planets(Count - 1, NewPlanetStates).

% Helper function for generating random numbers in a range
random_between(Min, Max) ->
    Min + trunc(rand:uniform() * (Max - Min + 1)) - 1.


getNextPlanetStates(PlanetStates) ->
    NewPlanetStates = 
        lists:foldl(
        fun(Key, Acc) ->
            Planet = maps:get(Key, PlanetStates),
            NewPlanet = nextPlanetPos(Planet),
            maps:put(Key, NewPlanet, Acc)
        end,
        PlanetStates,
        maps:keys(PlanetStates)
    ),
    NewPlanetStates.

nextPlanetPos({Pos, Vel}) -> %% TODO - ADJUST VALUES MAGNITUDE AND VELOCITY LIMIT
    SunPos = #pvector{x = 1980/2,y=1080/2},
    Accel = pvector_sub(SunPos, Pos), % Get the vector from the player to the sun
    Accel1 = set_magnitude(Accel, 0.08), %% TODO - Change the magnitude after testing, planet should be floating more
    NewVel = pvector_add(Vel, Accel1),
    NewLimitedVel = pvector_limit(NewVel, 6), % Planets should be a little slower than the player ? 
    NewPos = pvector_add(Pos, NewLimitedVel),
    {NewPos, NewLimitedVel}.
