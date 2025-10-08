-module(accounts).
-include("server.hrl").
-export([start/2]).

start(Accs, Lvl) -> accounts(maps:new(), Accs, Lvl).

accounts(SPids, Accs, Lvl) ->
    io:format("Accounts ~p~n ~p~n ~p~n", [SPids, Accs, Lvl]),
    receive
        {save_acc, DAccs} -> 
            offProc ! {full_save, "accounts.bin", DAccs},
            accounts(SPids, Accs, Lvl);
        {save_lvl, DLvl} -> 
            offProc ! {full_save, "levels.bin", DLvl},
            accounts(SPids, Accs, Lvl);
        {shutdown} -> 
            offProc ! {full_save, "levels.bin", Lvl},
            offProc ! {full_save, "accounts.bin", Accs};
        {online, Pid} ->
            NewPids = maps:put(Pid, "Anonymous", SPids),
            accounts(NewPids, Accs, Lvl);
        {create_account, Username, Password, Pid} ->
            case maps:is_key(Username, Accs) of
                true ->
                    ?SEND_MESSAGE(Pid, "res@@@Nome ja existe\n"),
                    accounts(SPids, Accs, Lvl);
                false ->
                    NAccs = maps:put(Username, Password, Accs),
                    NLvl = maps:put(Username, {0,0}, Lvl),
                    NewPids = maps:put(Pid, Username, SPids),
                    self() ! {save_acc, NAccs},
                    self() ! {save_lvl, NLvl},
                    ?SEND_MESSAGE(Pid, "res@@@success\n"),
                    accounts(NewPids, NAccs, NLvl)
            end;
        {login, Username, Password, Pid} ->
            case maps:is_key(Username, Accs) of
                true ->
                    RealPass = maps:get(Username, Accs),
                    if RealPass == Password ->
                        NewPids = maps:put(Pid, Username, SPids),
                        ?CHANGE_STATE(Pid, {new_name, Username}),
                        ?SEND_MESSAGE(Pid, "res@@@success\n"),
                        accounts(NewPids, Accs, Lvl);
                    true ->
                        ?SEND_MESSAGE(Pid, "res@@@Password incorreta\n"),
                        accounts(SPids, Accs, Lvl)
                    end;
                false ->
                    ?SEND_MESSAGE(Pid, "res@@@Username nao existe\n"),
                    accounts(SPids, Accs, Lvl)
            end;
        {logout, Pid} ->
            User = maps:get(Pid, SPids),
            if User == "Anonymous" ->
                ?SEND_MESSAGE(Pid, "res@@@Logout ja feito\n"),
                accounts(SPids, Accs, Lvl);
            true->
                NewPids = maps:put(Pid, "Anonymous", SPids),
                ?CHANGE_STATE(Pid, {new_room, "main"}),
                ?CHANGE_STATE(Pid, {new_name, "Anonymous"}),
                ?SEND_MESSAGE(Pid, "res@@@success\n"),
                accounts(NewPids, Accs, Lvl)
            end;
        {change_name, Name, Pid} -> 
            User = maps:get(Pid, SPids),
            if User == "Anonymous" ->
                ?SEND_MESSAGE(Pid, "res@@@Precisas de fazer login\n"),
                accounts(SPids, Accs, Lvl);
            true ->
                case maps:is_key(Name, Accs) of
                    true ->
                        ?SEND_MESSAGE(Pid, "res@@@Nome ja existe\n"),
                        accounts(SPids, Accs, Lvl);
                    false ->
                        ?CHANGE_STATE(Pid, {new_name, Name}),
                        NSocket = maps:put(Pid, Name, SPids),
                        Pass = maps:get(User, Accs),
                        NAccs = maps:put(Name, Pass, maps:remove(User, Accs)),
                        self() ! {save_acc, NAccs},
                        ?SEND_MESSAGE(Pid, "res@@@Nome Mudado\n"),
                        accounts(NSocket, NAccs, Lvl)
                end
            end;
        {change_pass, Pass, Pid} -> 
            User = maps:get(Pid, SPids),
            if User == "Anonymous" ->
                ?SEND_MESSAGE(Pid, "res@@@Precisas de fazer login\n"),
                accounts(SPids, Accs, Lvl);
            true ->
                NAccs = maps:put(User, Pass, Accs),
                self() ! {save_acc, NAccs},
                ?SEND_MESSAGE(Pid, "res@@@Pass Mudada\n"),
                accounts(SPids, NAccs, Lvl)
            end;
        {remove_account, Pid} ->
            User = maps:get(Pid, SPids),
            if User == "Anonymous" ->
                ?SEND_MESSAGE(Pid, "res@@@Precisas de fazer login\n"),
                accounts(SPids, Accs, Lvl);
            true ->
                NAccs = maps:remove(User, Accs),
                NSocket = maps:put(Pid, "Anonymous", SPids),
                self() ! {save_acc, NAccs},
                ?CHANGE_STATE(Pid, {new_name, "Anonymous"}),
                ?CHANGE_STATE(Pid, {new_room, "main"}),
                ?SEND_MESSAGE(Pid, "res@@@Conta Removida\n"),
                accounts(NSocket, NAccs, Lvl)
            end;
        {ranking, Pid} ->
            RankedList = lists:sort(fun({_, {Lvl1, XP1}}, {_, {Lvl2, XP2}}) ->
                                        case Lvl1 =:= Lvl2 of
                                            true -> XP1 > XP2;
                                            false -> Lvl1 > Lvl2
                                        end
                                    end, maps:to_list(Lvl)),
            Ranking = lists:map(fun({Username, _}) -> Username end, RankedList),
            ?SEND_MUL_MESSAGE(Pid, Ranking),
            accounts(SPids, Accs, Lvl);
        {update_lvl, Pid, NewLvl, NewXP} ->
            ?CHANGE_STATE(Pid, {new_lvl, NewLvl}),
            ?CHANGE_STATE(Pid, {new_xp, NewXP}),
            NLvl = maps:put(maps:get(Pid, SPids), {NewLvl, NewXP}, Lvl),
            self() ! {save_lvl, NLvl},
            accounts(SPids, Accs, NLvl);
        {offline, Pid} ->
            NewPids = maps:remove(Pid, SPids),
            accounts(NewPids, Accs, Lvl);
        Other ->
            io:fwrite("accounts.erl ERROR ~p ~p\n", [self(), Other]),
            accounts(SPids, Accs, Lvl)
    end.