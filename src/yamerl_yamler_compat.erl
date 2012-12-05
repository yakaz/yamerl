-module(yamerl_yamler_compat).

-include("yamerl_errors.hrl").

%% Public API.
-export([
    load/1,
    load/2,
    load_file/1,
    load_file/2,
    convert_options/1,
    convert_options/2
  ]).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

load(String) ->
    load(String, []).

load(String, Options) ->
    Options1 = convert_options(Options),
    try
        Result = yamerl_constr:string(String, Options1),
        {ok, Result}
    catch
        throw:#yamerl_exception{errors = [Error]} ->
            format_error(Error)
    end.

load_file(Filename) ->
    load_file(Filename, []).

load_file(Filename, Options) ->
    Options1 = convert_options(Options),
    try
        Result = yamerl_constr:file(Filename, Options1),
        {ok, Result}
    catch
        throw:#yamerl_exception{errors = [Error]} ->
            format_error(Error)
    end.

convert_options(Options) ->
    convert_options(Options, []).

convert_options([{schema, yaml_schema_failsafe} | Rest], Converted) ->
    Converted1 = [
      {schema, failsafe}
      | Converted
    ],
    convert_options(Rest, Converted1);
convert_options([{schema, yaml_schema_json} | Rest], Converted) ->
    Converted1 = [
      {schema, json}
      | Converted
    ],
    convert_options(Rest, Converted1);
convert_options([{schema, yaml_schema_core} | Rest], Converted) ->
    Converted1 = [
      {schema, yaml11}
      | Converted
    ],
    convert_options(Rest, Converted1);
convert_options([{schema, yaml_schema_erlang} | Rest], Converted) ->
    Converted1 = [
      {schema, yaml11},
      {node_mods, [yamerl_node_erlang_atom]}
      | Converted
    ],
    convert_options(Rest, Converted1);
convert_options([implicit_atoms | Rest], Converted) ->
    Converted1 = [
      {node_mods, [yamerl_node_erlang_atom]},
      erlang_atom_autodetection
      | Converted
    ],
    convert_options(Rest, Converted1);
convert_options([{implicit_atoms, true} | Rest], Converted) ->
    Converted1 = [
      {node_mods, [yamerl_node_erlang_atom]},
      erlang_atom_autodetection
      | Converted
    ],
    convert_options(Rest, Converted1);
convert_options([], Converted) ->
    Converted1 = case proplists:get_value(schema, Converted) of
        undefined -> [{schema, yaml11} | Converted];
        _         -> Converted
    end,
    [
      {doc_version, {1, 1}},
      simple_structs,
      str_node_as_binary,
      inf_float_node_like_yamler
      | Converted1
    ].

format_error(#yamerl_parsing_error{text = Text, line = Line, column = Col}) ->
    Message = lists:flatten(
      io_lib:format("~s, Line ~b, Column ~b", [Text, Line, Col])),
    {error, Message};
format_error(#yamerl_invalid_option{text = Text}) ->
    {error, Text}.
