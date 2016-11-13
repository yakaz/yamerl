-module('ex_8.10_folded_lines').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        88,
        true,
        [],
        0,
        89,
        17,
        1,
        false,
        16,
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
        [{impl_key,false,false,undefined,undefined,1,1}],
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
          {yamerl_stream_end,16,10},
          {yamerl_doc_end,16,10},
          {yamerl_scalar,1,1,
            {yamerl_tag,1,1,{non_specific,"!"}},
            block,folded,
            "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n"},
          {yamerl_doc_start,1,1,{1,2},_},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
