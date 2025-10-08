-module(game_player).
-export([start/4]).
-include("server.hrl").
-include("pvectors.hrl").

-define(START_POS, [#pvector{x=100,y=100},#pvector{x=1500,y=1000}, #pvector{x=100,y=1000}, #pvector{x=1500,y=100}]). % All possible starting positions for players
-define(SUN_RADIUS, 135). % Radius of sun
-define(PLANET_RADIUS, 100). % Radius of planets
-define(SUN_POS, #pvector{x=1920/2,y=1080/2}). % Position of sun
-define(TOP_SPEED, 10). % Maximum speed of player
-define(ACCEL_MAG, 0.1). % Magnitude of acceleration
-define(PLAYER_RADIUS, 25).  % Radius of player

start(GameProc, Sock, UserAuth, PlayerNum) -> %PlayerState = {{Name, Level, Lobby, XP}, BoostLeft, {Pos, Velocity, Angle}, Buttons}
    %io:format("game proc pid ~p\n", [GameProc]),
    {Name, _, _, _} = UserAuth,
    Keys = {false, false, false},
    %io:format("~p\n", [Keys]),
    Me = self(),
    %io:format("Sending new_pid to gameProc ~p~p~p\n", [Name,Me,PlayerNum]),
    GameProc ! {new_pid, Name, Me, PlayerNum},
    gamePlayer(GameProc, Sock, {UserAuth, 100, {#pvector{x=0,y=0}, #pvector{x=0,y=0},#pvector{x=0,y=0},0}, Keys},0). 

gamePlayer(GameProc, Sock, PlayerState, PlayerIndex) ->
    receive
        {broadcast, Data} ->
            %io:format("Sending broadcast ~p\n", [Data]),
            ?SEND_BROADCAST(Sock, Data),
            gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
        {broadcast_list, Data} ->
            ?SEND_BROADCAST_LIST(Sock, Data),
            gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
        {start_pos, Index} -> % só é chamado uma vez para a posiçao inicial
            NewState = setupPlayerState(PlayerState, Index), 
            %io:format("Starting state ~p\n", [NewState]),
            GameProc ! {starting_pos, self(), NewState, Index},
            %{{Name,_,_,_}, Boost, {VecPos, _,_, _}, _} = NewState,
            % Mensagem pode ser imediatamente enviada ao jogador. maybe. or wait until all player states are set up
            %?SEND_MESSAGE(Sock, "state\n" ++ NewState ++ "\n"),
            gamePlayer(GameProc, Sock, NewState, Index);
        {player_state} ->
            NewPos = getNextPos(PlayerState),
            %io:format("Player ~p => state ~p\n", [self()], NewPos),
            Bool = checkCollision(NewPos),
            case Bool of 
                true -> % if the player collides with the sun or the screen
                    Me = self(),
                    GameProc ! {died, Me},
                    io:format("Position of death: ~p\n", [NewPos]);
                false -> % if the player does not collide with the sun or the screen
                    GameProc ! {player_state, self(), NewPos, PlayerIndex}
            end,
            gamePlayer(GameProc, Sock, NewPos, PlayerIndex);
        {fpieces, Data} ->
            case Data of
                {died} ->
                    io:format("Died ~p\n", [self()]),
                    ?SEND_BROADCAST(Sock, "game@@@you_died\n"),
                    gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
                {unexpected_leave} ->
                    self() ! {fpieces, {end_game}},
                    gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
                {won} ->
                    {{Name, Lvl, Lobby, XP}, Boost, Pos, But} = PlayerState,
                    if XP < 1 ->
                        NXP = 1;
                    true ->
                        NXP = XP + 1
                    end,
                    if Lvl == NXP ->
                        NLvl = Lvl + 1;
                    true ->
                        NLvl = Lvl
                    end,
                    gamePlayer(GameProc, Sock, {{Name, NLvl, Lobby, NXP}, Boost, Pos, But}, PlayerIndex);
                {lost} ->
                    {{Name, Lvl, Lobby, XP}, Boost, Pos, But} = PlayerState,
                    if XP > - 1 ->
                        NXP = - 1;
                    true ->
                        NXP = XP - 1
                    end,
                    if Lvl/2 == - NXP ->
                        if Lvl == 1 ->
                            NLvl = 1;
                        true ->
                            NLvl = Lvl - 1
                        end;
                    true ->
                        NLvl = Lvl
                    end,
                    gamePlayer(GameProc, Sock, {{Name, NLvl, Lobby, NXP}, Boost, Pos, But}, PlayerIndex);
                {end_game} ->
                    {{Name, Level, _, XP}, _, _, _} = PlayerState,
                    accsProc ! {update_lvl, self(), Level, XP},
                    String = "lvl@@@" ++ erlang:integer_to_list(Level) ++ "@@@XP@@@" ++ erlang:integer_to_list(XP) ++ "\n",
                    io:fwrite("Sending ~p\n", [String]),
                    ?SEND_MESSAGE(self(), String),
                    server:userAuth(Sock, {Name, Level, "main", XP})
            end;
        {tcp, _, Data} ->
            case re:split(binary_to_list(Data), "@@@") of
                [<<?CHAT_MESSAGE>>, Message] -> % sends a chat message
                    GameProc ! {send_message, 
                        self(), 
                        string:trim(binary_to_list(Message), trailing)},
                    gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
                [<<?LEAVE_CHAT>>, _] -> % sends a chat message
                    GameProc ! {leave_chat, self()},
                    gamePlayer(GameProc, Sock, PlayerState, PlayerIndex);
                [<<?UP_KEY>>, _] -> % sum about new button pressed (?)
                    {Name, Boost, {Pos, Vel, Acc, Angle}, {UP, LEFT, RIGHT}} = PlayerState,
                    NewKeys = {toggle(UP), LEFT, RIGHT},
                    NewPlayerState = {Name, Boost, {Pos, Vel, Acc, Angle}, NewKeys},
                    % as if the calculations made the player die;
                    %GameProc ! {died, self()}, % !!!! ONLY FOR TESTING DEATH/XP this is supposed to be handled by the game_sim, not the player
                    gamePlayer(GameProc, Sock, NewPlayerState, PlayerIndex);
                [<<?RIGHT_KEY>>, _] -> % sum about new button pressed (?)
                    {Name, Boost, {Pos, Vel, Acc, Angle}, {UP, LEFT, RIGHT}} = PlayerState,
                    NewKeys = {UP, LEFT, toggle(RIGHT)},
                    NewPlayerState = {Name, Boost, {Pos, Vel, Acc, Angle}, NewKeys},
                    gamePlayer(GameProc, Sock, NewPlayerState, PlayerIndex);
                [<<?LEFT_KEY>>, _] -> % sum about new button pressed (?)
                    {Name, Boost, {Pos, Vel, Acc, Angle}, {UP, LEFT, RIGHT}} = PlayerState,
                    NewKeys = {UP, toggle(LEFT), RIGHT},
                    NewPlayerState = {Name, Boost, {Pos, Vel, Acc, Angle}, NewKeys},
                    gamePlayer(GameProc, Sock, NewPlayerState, PlayerIndex)
            end;
        {tcp_closed, _} ->
            {{_, _, Lobby, _}, _, _, _} = PlayerState,
            lobbyProc ! {offline, Lobby, self()},
            accsProc ! {offline, self()},
            GameProc ! {interrupt_game, self()};
        {tcp_error, _, _} ->
            {{_, _, Lobby, _}, _, _, _} = PlayerState,
            lobbyProc ! {offline, Lobby, self()},
            accsProc ! {offline, self()},
            GameProc ! {interrupt_game, self()};
        Other ->
            io:fwrite("ERROR ~p ~p\n", [self(), Other]),
            gamePlayer(GameProc, Sock, PlayerState, PlayerIndex)
    end.


% helper function to toggle a boolean value - why does erlang not have a ! operator???
toggle(true) -> false;
toggle(false) -> true.

%% Initialize a player state with a given index Playerpos
setupPlayerState(PlayerState, Playerpos) -> % I need the player position. Player 1 gets position #1 ... Player 4 gets position #4
    io:format("Attempting to get playerPos ~p\n", [Playerpos]),
    InitialPos = lists:nth(Playerpos, ?START_POS),
    Vec = #pvector{x = 0.0, y = 0.0},
    KeyMap = {false, true, false}, %default keymap {up,left,right}
    {Name, _, _, _} = PlayerState,
    PlayerState1 = {Name, 100,   {InitialPos,   Vec,        Vec,       0},   KeyMap},
    %             {Username?, Boost,  {{PX,PY},   {VX,VY}  ,{AccX,AccY}, Angle}, Keys}
    PlayerState1.


getNextPos(PlayerState) ->
    Sunpos = ?SUN_POS,
    {Name, Boost, {Pos, Vel, Acc, Angle}, {UP,LEFT,RIGHT} = KeyMap} = PlayerState,
    AccMag = ?ACCEL_MAG,
    Topspeed = ?TOP_SPEED,
    Accel = pvector_sub(Sunpos, Pos), % Get the vector from the player to the sun
    Accel1 = set_magnitude(Accel, AccMag), % Limit the magnitude of the acceleration vector to 0.1
    KeyAccel = #pvector{x=0,y=0},
    case Boost of
        0 -> 
            Acc___ = KeyAccel,
            Boost___ = 0;
        _ -> 
            case UP of
                true -> 
                    AngleMovement = #pvector{x=math:cos(Angle) * 0.2, y = math:sin(Angle)*0.2},  % Calculate velocity vector for a given angle
                    Acc_ = pvector_add(KeyAccel, AngleMovement),
                    Boost_ = Boost - 1;                                                    % Add the key acceleration to the angle movement
                _ -> 
                    Acc_ = KeyAccel,
                    Boost_ = Boost - 1
                    
            end,
            case LEFT of
                true ->
                    % Calculate velocity vector for a given angle
                    AngleMovement1 = #pvector{x=math:cos(Angle + math:pi()/2) * 0.2, y = math:sin(Angle + math:pi()/2) * 0.2},
                    Acc__ = pvector_add(Acc, AngleMovement1), % Add the key acceleration to the angle movement
                    Boost__ = Boost_ - 1;
                _ ->
                    Acc__ = Acc_,
                    Boost__ = Boost_ - 1
            end,
            case RIGHT of
                true ->
                    % Calculate velocity vector for a given angle
                    AngleMovement2 = #pvector{x=math:cos(Angle - math:pi()/2) * 0.2, y = math:sin(Angle - math:pi()/2) * 0.2},
                    Acc___= pvector_add(Acc, AngleMovement2), % Add the key acceleration to the angle movement
                    Boost___ = Boost__ - 1;
                _ ->
                    Acc___ = Acc__,
                    Boost___ = Boost__ - 1
            end
    end,
    
    FinalAccel = pvector_add(Accel1, Acc___), % Add the key acceleration to the angle movement
    Velocity = pvector_add(FinalAccel, Vel), % Finally, add accel to velocity
    LimitVel = pvector_limit(Velocity, Topspeed), % Limit the velocity to the top speed
    NewPos = pvector_add(LimitVel, Pos), % Add velocity to position

    NewPlayerState = {Name, Boost___, {NewPos, LimitVel, FinalAccel, Angle}, KeyMap},
    NewPlayerState.


checkCollision(PlayerState) ->
    {_, _, {Pos, _, _, _}, _} = PlayerState,
    Sunpos = ?SUN_POS,
    Sunrad = ?SUN_RADIUS,
    Playrad = ?PLAYER_RADIUS,
    SunDist = pvector_dist(Pos, Sunpos),
    SunCollide = SunDist < Playrad + (Sunrad - 20), %% Give the player some leeway man

    % Check for margin collisions with the screen
    %% these values are for 1920x1080 screen... I should probably ask the client for displayWidth/displayHeight
    %% or just force the client to be 1920x1080 :)
    CollideX = case Pos#pvector.x of 
                   X when X < -10 ->  
                       true;
                   X when X > 1930 ->
                       true;
                   _ ->
                       false
               end,
    CollideY = case Pos#pvector.y of 
                   Y when Y < -10 -> 
                       true;
                   Y when Y > 1090 ->
                       true;
                   _ ->
                       false
               end,
    
    % Check for collision with the sun or the screen
    SunCollide or CollideX or CollideY.