-module(offline).
-export([start/0]).

start() ->
    offline(false).

offline(Saved) ->
    receive %save, full_save, load
        {load, Pid} ->  
            
            case file:read_file("accounts.bin") of
                {ok, Binary} ->
                    Accs = binary_to_term(Binary);
                {error, Reason} ->
                    if Reason == enoent ->
                        Accs = #{"Anonymous" => "admin"};
                    true ->
                        Accs = maps:new(),
                        Pid ! {error, Reason},
                        io:format("Error reading file")
                    end
            end,
            case file:read_file("levels.bin") of
                {ok, BinaryLvl} ->
                    Lvl = binary_to_term(BinaryLvl),
                    Pid ! {loaded, Accs, Lvl};
                {error, ReasonLvl} ->
                    if ReasonLvl == enoent ->
                        Pid ! {loaded, Accs, #{"Anonymous" => {0,0}}};
                    true ->
                        Pid ! {error, ReasonLvl},
                        io:format("Error reading file")
                    end
            end,
            offline(false);   
        {full_save, File, Accs} -> %"accounts.bin"
            Binary = term_to_binary(Accs),
            Ret = file:write_file(File, Binary),
            io:format("Saved ~p\n", [Ret]),
            offline(true);
        off -> 
            if Saved -> 
                ok;
            true ->
                offline(Saved)
            end
    end.