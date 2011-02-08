%% $Id: bones_version.erl 2899 2010-05-07 15:06:08Z jean.sebastien.pedron $

-module(${MODULE}).

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
    Dir         = os:getenv("srcdir"),
    Include_Dir = filename:join([Dir, "..", "include"]),
    Src_Dir     = filename:join([Dir, "..", "src"]),
    Cover_To_Html = filename:join([Dir, "cover_to_html.sh"]),
    cover:start(),
    cover:compile_directory(Src_Dir, [{i, Include_Dir}]),
    Cover_To_Html.

cleanup(Cover_To_Html) ->
    Covered_Mod = list_to_atom(string:substr(atom_to_list(?MODULE), 3)),
    print_coverage(Cover_To_Html, [Covered_Mod]),
    cover:stop().

print_coverage(Cover_To_Html, Modules) ->
    io:format(standard_error, "  Coverage:~n", []),
    print_coverage2(Cover_To_Html, Modules).

print_coverage2(Cover_To_Html, [Mod | Rest]) ->
    {ok, {_Module, {Cov, Not_Cov}}} = cover:analyse(Mod, module),
    Mod_S = atom_to_list(Mod),
    if
        Cov > 0 andalso Not_Cov > 0 ->
            file:write_file("cover_" ++ Mod_S ++ ".percent",
              list_to_binary(io_lib:format("~.1f",
                  [Cov * 100 / (Cov + Not_Cov)]))),
            io:format(standard_error, "   - ~s: ~.1f%~n",
              [Mod, Cov * 100 / (Cov + Not_Cov)]);
        true ->
            io:format(standard_error, "   - ~s: n/a~n~n", [Mod])
    end,
    cover:analyse_to_file(Mod, "cover_" ++ Mod_S ++ ".out", []),
    os:cmd(Cover_To_Html ++ " " ++ Mod_S),
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
