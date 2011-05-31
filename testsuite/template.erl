%% $Id: bones_version.erl 2899 2010-05-07 15:06:08Z jean.sebastien.pedron $

-module('${MODULE}').

-include_lib("eunit/include/eunit.hrl").

%% -------------------------------------------------------------------
%% Testsuite entry point.
%% -------------------------------------------------------------------

main_test_() ->
    Setup   = fun setup/0,
    Cleanup = fun cleanup/1,
    Tests   = tests_list(),
    {setup, Setup, Cleanup, Tests}.

%% -------------------------------------------------------------------
%% Setup and cleanup.
%% -------------------------------------------------------------------

setup() ->
    %% Setup coverity checking.
    Dir           = os:getenv("srcdir"),
    Include_Dir   = filename:join([Dir, "..", "include"]),
    Src_Dir       = filename:join([Dir, "..", "src"]),
    Cover_To_Html = filename:join([Dir, "cover_to_html.sh"]),
    Mods_List     = filename:join([Dir, "data", ?MODULE_STRING,
        "COVERED-MODS"]),
    cover:start(),
    Covered_Mods  = case file:consult(Mods_List) of
        {ok, [ML]} ->
            Fun = fun(M) ->
                F = filename:join([Src_Dir, M]) ++ ".erl",
                cover:compile_module(F, [{i, Include_Dir}])
            end,
            lists:foreach(Fun, ML),
            ML;
        _ ->
            []
    end,
    {Covered_Mods, Cover_To_Html}.

cleanup({Covered_Mods, Cover_To_Html}) ->
    print_coverage(Cover_To_Html, Covered_Mods),
    cover:stop().

print_coverage(Cover_To_Html, Modules) ->
    Name  = string:to_upper([hd(?MODULE_STRING)]) ++ tl(?MODULE_STRING),
    Name1 = string:join(string:tokens(Name, "_"), " "),
    io:format(standard_error, "  ~s / Coverage:~n", [Name1]),
    print_coverage2(Cover_To_Html, Modules).

print_coverage2(Cover_To_Html, [Mod | Rest]) ->
    {ok, {_Module, {Cov, Not_Cov}}} = cover:analyse(Mod, module),
    Mod_S = atom_to_list(Mod),
    if
        Cov > 0 orelse Not_Cov > 0 ->
            file:write_file("cover_" ?MODULE_STRING "_" ++ Mod_S ++ ".percent",
              list_to_binary(io_lib:format("~.1f~n",
                  [Cov * 100 / (Cov + Not_Cov)]))),
            io:format(standard_error, "   - ~s: ~.1f%~n",
              [Mod, Cov * 100 / (Cov + Not_Cov)]);
        true ->
            file:write_file("cover_" ?MODULE_STRING "_" ++ Mod_S ++ ".percent",
              list_to_binary(io_lib:format("0.0~n",
                  []))),
            io:format(standard_error, "   - ~s: n/a~n", [Mod])
    end,
    cover:analyse_to_file(Mod,
      "cover_" ?MODULE_STRING "_" ++ Mod_S ++ ".out", []),
    os:cmd(Cover_To_Html ++ " " ?MODULE_STRING " " ++ Mod_S),
    print_coverage2(Cover_To_Html, Rest);
print_coverage2(_, []) ->
    ok.

%% -------------------------------------------------------------------
%% Tests listing and construction.
%% -------------------------------------------------------------------

tests_list() ->
    [
      %% DO NOT EDIT: The list of tests is filled automatically using
      %% the files in data/$module/.
    ].
