-module('${MODULE}').

-include_lib("eunit/include/eunit.hrl").

-define(srcdir,       "${srcdir}").
-define(builddir,     "${builddir}").
-define(top_srcdir,   "${top_srcdir}").
-define(top_builddir, "${top_builddir}").

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
    %% Setup coverity checking. To know what module to compile and the
    %% options to use, we look at the Emakefile.
    {ok, Emakefile} = file:consult(
      filename:join([?top_builddir, "src", "Emakefile"])),
    cover:start(),
    cover_compile(Emakefile).

cover_compile([{Mods, Options} | Rest]) ->
    cover_compile2(Mods, Options),
    cover_compile(Rest);
cover_compile([]) ->
    ok.

cover_compile2([Mod | Rest], Options) ->
    File = atom_to_list(Mod) ++ ".erl",
    case cover:compile(File, Options) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            io:format(standard_error,
              "Failed to compile \"~s\": ~p~n", [File, Reason])
    end,
    cover_compile2(Rest, Options);
cover_compile2([], _) ->
    ok.

cleanup(_) ->
    cover:export(?MODULE_STRING ".coverdata"),
    cover:stop().

%% -------------------------------------------------------------------
%% Tests listing and construction.
%% -------------------------------------------------------------------

tests_list() ->
    [
      %% DO NOT EDIT: The list of tests is filled automatically using
      %% the files in data/$module/.
    ].
