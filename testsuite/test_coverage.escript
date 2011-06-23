#!/usr/bin/env escript
%%! +K true -smp enable
%% vim:ft=erlang:

main([Prefix | Exports_And_Mods]) ->
    cover:start(),
    load_exports(Prefix, Exports_And_Mods).

load_exports(Prefix, ["--" | Mods]) ->
    analyze_mods(Prefix, Mods);
load_exports(Prefix, [Export | Rest]) ->
    cover:import(Export),
    load_exports(Prefix, Rest);
load_exports(Prefix, []) ->
    analyze_mods(Prefix, cover:imported_modules()).

analyze_mods(Prefix, Mods) ->
    Out = Prefix ++ "_covered_mods",
    case file:open(Out, [write]) of
        {ok, FD} ->
            Parent = self(),
            Procs  = [
              erlang:spawn_link(fun() -> analyze_mod(Parent, Prefix, Mod) end)
              || Mod <- Mods
            ],
            wait_for_children(FD, Procs);
        {error, Reason} ->
            io:format(standard_error,
              "Failed to open \"~s\": ~p~n", [Out, Reason]),
            halt(1)
    end.

wait_for_children(FD, []) ->
    file:close(FD);
wait_for_children(FD, Procs) ->
    receive
        {done, Child, Mod} ->
            io:format(FD, "~s~n", [Mod]),
            Procs1 = Procs -- [Child],
            wait_for_children(FD, Procs1);
        {'EXIT', Child} ->
            Procs1 = Procs -- [Child],
            wait_for_children(FD, Procs1)
    end.

analyze_mod(Parent, Prefix, Mod) ->
    %% This function write two files:
    %%     o  Module.percent: a single line with the cover percentage.
    %%     o  Module.out: the source file with the number of passes for
    %%        each line.
    Out = Prefix ++ "_" ++ atom_to_list(Mod),
    case cover:analyse(Mod, module) of
        {ok, {_, {Cov, Not_Cov}}} ->
            if
                Cov > 0 orelse Not_Cov > 0 ->
                    file:write_file(Out ++ ".percent",
                      list_to_binary(io_lib:format("~.1f~n",
                          [Cov * 100 / (Cov + Not_Cov)])));
                true ->
                    file:write_file(Out ++ ".percent",
                      list_to_binary(io_lib:format("0.0~n",
                          [])))
            end,
            case cover:analyse_to_file(Mod, Out ++ ".out") of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    io:format(standard_error,
                      "Failed to analyse module \"~s\" source: ~p~n",
                      [Mod, Reason])
            end;
        {error, Reason} ->
            io:format(standard_error,
              "Failed to analyse module \"~s\": ~p~n", [Mod, Reason])
    end,
    Parent ! {done, self(), Mod}.
