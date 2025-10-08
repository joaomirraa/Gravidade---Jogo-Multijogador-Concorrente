-module(login_manager).
-export([start/0, 
        create_account/2,
        close_account/2,
        login/2,
        logout/2,
        online/0]).

start() -> 
    Pid = spawn(fun() -> loop(#{}) end),
    register(login_manager, Pid).

create_account(User, Pass) ->
    Msg = {create_account, User, Pass, self()},
    login_manager ! Msg,
    receive
        {Res, login_manager} -> Res 
    end.

close_account(User, Pass) ->
    Msg = {close_account, User, Pass, self()},
    login_manager ! Msg,
    receive
        {Res, login_manager} -> Res
    end.

loop(Map) ->
    receive
        {create_account, User, Pass, From} ->
            Resposta = case maps:is_key(User, Map) of
                true -> {user_exists, login_manager};
                false -> From ! {ok, login_manager},
                         loop(maps:put(User, {Pass, true}, Map))
            end,

        {close_account, User, Pass, From} ->
            Resposta = case maps:find(User, Map) of
                {ok, {Passwd, _}} when Pass == Passwd ->
                    From ! {ok, login_manager},
                    loop(maps:remove(User, Map));
                _ ->
                    From ! {invalid_user, login_manager}
                    loop(Map)
            end,
