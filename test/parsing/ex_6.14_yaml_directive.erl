-module('ex_6.14_yaml_directive').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        66,
        true,
        [],
        0,
        67,
        5,
        1,
        false,
        4,
        6,
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
        7,
        6,
        undefined,
        undefined,
        _,
        false,
        [
          {yamerl_parsing_error,warning,
            "Version 1.3 not supported (maximum version 1.2); parsing may fail",
            3,1,version_not_supported,
            {yamerl_doc_start,3,1,{1,3},_},
            []}
        ],
        [
          {yamerl_stream_end,4,6},
          {yamerl_doc_end,4,6},
          {yamerl_scalar,4,1,
            {yamerl_tag,4,1,{non_specific,"!"}},
            flow,double_quoted,"foo"},
          {yamerl_doc_start,3,1,{1,3},_},
          {yamerl_yaml_directive,1,1,{1,3}},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
