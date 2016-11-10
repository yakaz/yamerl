-module('ex_6.13_reserved_directives').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        77,
        true,
        [],
        0,
        78,
        4,
        1,
        false,
        3,
        10,
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
            "Reserved directive \"FOO\" ignored",
            1,1,reserved_directive,
            {yamerl_reserved_directive,1,1,"FOO",["bar","baz"],2},
            []}
        ],
        [
          {yamerl_stream_end,3,10},
          {yamerl_doc_end,3,10},
          {yamerl_scalar,3,5,
            {yamerl_tag,3,5,{non_specific,"!"}},
            flow,double_quoted,"foo"},
          {yamerl_doc_start,3,1,{1,2},_},
          {yamerl_reserved_directive,1,1,"FOO",["bar","baz"],2},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
