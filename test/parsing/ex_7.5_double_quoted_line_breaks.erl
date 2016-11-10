-module('ex_7.5_double_quoted_line_breaks').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        63,
        true,
        [],
        0,
        64,
        6,
        1,
        false,
        5,
        17,
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
        6,
        5,
        undefined,
        undefined,
        _,
        false,
        [],
        [
          {yamerl_stream_end,5,17},
          {yamerl_doc_end,5,17},
          {yamerl_scalar,1,1,
            {yamerl_tag,1,1,{non_specific,"!"}},
            flow,double_quoted,
            "folded to a space,\nto a line feed, or \t \tnon-content"},
          {yamerl_doc_start,1,1,{1,2},_},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
