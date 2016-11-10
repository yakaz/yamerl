-module('ex_6.15_invalid_repeated_yaml_directive').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        24,
        true,
        [],
        0,
        25,
        4,
        1,
        false,
        3,
        4,
        utf8,
        false,
        undefined,
        _,
        _,
        [],
        {bcoll,root,0,-1,1,1,-1,1,1},
        false,
        false,
        false,
        [{impl_key,false,undefined,undefined,undefined,undefined,undefined}],
        false,
        false,
        _,
        [],
        0,
        8,
        7,
        undefined,
        undefined,
        _,
        false,
        [
          {yamerl_parsing_error,warning,
            "Multiple YAML directives found: the last one will be used",
            2,1,multiple_yaml_directives,
            {yamerl_yaml_directive,2,1,{1,1}},
            []}
        ],
        [
          {yamerl_stream_end,3,4},
          {yamerl_doc_end,3,4},
          {yamerl_scalar,3,1,
            {yamerl_tag,3,1,{non_specific,"?"}},
            flow,plain,"foo"},
          {yamerl_doc_start,3,1,{1,1},_},
          {yamerl_yaml_directive,2,1,{1,1}},
          {yamerl_yaml_directive,1,1,{1,2}},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
