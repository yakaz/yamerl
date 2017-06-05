-module(document_delimiters).

-include_lib("eunit/include/eunit.hrl").

%% This file contains testcases provided by Barry Allard when he
%% reported #18. Before the bugfix, they would trigger an infinite loop
%% in the parser.
%%
%% FIXME: It's still unclear if the returned values are correct for the
%% given documents.

setup() ->
    application:start(yamerl).

parse_blank_string_test_() ->
    {setup,
     fun setup/0,
     [
      ?_assertEqual([], yamerl_constr:string(""))
     ]
    }.

parse_empty_json_map_test_() ->
    {setup,
     fun setup/0,
     [
      ?_assertEqual([[]], yamerl_constr:string("{}"))
     ]
    }.

parse_dos_vuln_string_test_() ->
    %% TODO: Consider fuzzing using QuickCheck Mini, PropEr, etc.
    {setup,
     fun setup/0,
     [
      ?_assertEqual([null], yamerl_constr:string("---")),
      ?_assertEqual([], yamerl_constr:string("...")),
      ?_assertEqual([null], yamerl_constr:string("\n---")),
      ?_assertEqual([null], yamerl_constr:string("\r---")),
      ?_assertEqual([null], yamerl_constr:string("\r\n---")),
      ?_assertEqual([null, null], yamerl_constr:string("\n---\n---")),
      ?_assertEqual([null, null], yamerl_constr:string(" \n---\n---")),
      ?_assertEqual([null, null], yamerl_constr:string("\n---\n---\n...")),
      ?_assertEqual(["anything", null], yamerl_constr:string("anything\n---"))
     ]
    }.

parse_dos_vuln_bad_string_1_test_() ->
    {setup,
     fun setup/0,
     [
      ?_assertEqual(["--"], yamerl_constr:string("--"))
     ]
    }.

parse_dos_vuln_bad_string_2_test_() ->
    {setup,
     fun setup/0,
     [
      ?_assertEqual(["----"], yamerl_constr:string("----"))
     ]
    }.
