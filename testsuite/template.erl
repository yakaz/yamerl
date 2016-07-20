%-
% Copyright (c) 2012-2014 Yakaz
% Copyright (c) 2016 Jean-Sébastien Pédron <jean-sebastien.pedron@dumbbell.fr>
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
% 1. Redistributions of source code must retain the above copyright
%    notice, this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright
%    notice, this list of conditions and the following disclaimer in the
%    documentation and/or other materials provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
% SUCH DAMAGE.

-module('${MODULE}').

-include_lib("eunit/include/eunit.hrl").
${include_lib}
${include}

-define(srcdir,       "${srcdir}").
-define(builddir,     "${builddir}").
-define(top_srcdir,   "${top_srcdir}").
-define(top_builddir, "${top_builddir}").

%% -------------------------------------------------------------------
%% Testsuite entry point.
%% -------------------------------------------------------------------

-ifndef(TEST).
main_test_() ->
    Setup   = fun setup/0,
    Cleanup = fun cleanup/1,
    Tests   = tests_list(),
    {setup, Setup, Cleanup, Tests}.
-else.
main_test_() ->
    tests_list().
-endif.

%% -------------------------------------------------------------------
%% Setup and cleanup.
%% -------------------------------------------------------------------

-ifndef(TEST).
setup() ->
    %% Setup coverity checking. To know what module to compile and the
    %% options to use, we look at the Emakefile.
    {ok, Emakefile} = file:consult(
      filename:join([?top_builddir, "src", "Emakefile"])),
    cover:start(),
    cover_compile(Emakefile).

cover_compile([{Mods, Options} | Rest]) ->
    %% Include directories are relative to $top_builddir/src in
    %% src/Emakefile. We must prepend this path to each directory. If
    %% "Dir" is an absolute directory, filename:join/1 won't change it.
    Options1 = [
      case Option of
          {i, Dir} -> {i, filename:join([?top_builddir, "src", Dir])};
          _        -> Option
      end
      || Option <- Options
    ],
    cover_compile2(Mods, Options1),
    cover_compile(Rest);
cover_compile([]) ->
    ok.

cover_compile2([Mod | Rest], Options) ->
    File = filename:join([?top_builddir, "src", atom_to_list(Mod) ++ ".erl"]),
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
-else.
-endif.

%% -------------------------------------------------------------------
%% Tests listing and construction.
%% -------------------------------------------------------------------

tests_list() ->
    [
      %% DO NOT EDIT: The list of tests is filled automatically using
      %% the files in data/$module/.
    ].
