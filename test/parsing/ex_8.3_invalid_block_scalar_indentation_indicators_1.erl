-module('ex_8.3_invalid_block_scalar_indentation_indicators_1').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        13,
        true,
        [],
        0,
        14,
        4,
        1,
        false,
        4,
        1,
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
        9,
        8,
        undefined,
        undefined,
        _,
        false,
        [
          {yamerl_parsing_error,warning,
            "A leading all-space line has too many spaces (2) compared to detected indentation (1)",
            3,2,leading_empty_lines_too_long,
            {yamerl_scalar,1,3,{yamerl_tag,1,3,{non_specific,"!"}},block,literal,[]},
            []}
        ],
        [
          {yamerl_stream_end,4,1},
          {yamerl_doc_end,4,1},
          {yamerl_collection_end,4,1,block,sequence},
          {yamerl_scalar,1,3,
            {yamerl_tag,1,3,{non_specific,"!"}},
            block,literal,"\ntext\n"},
          {yamerl_sequence_entry,1,1},
          {yamerl_collection_start,1,1,
            {yamerl_tag,1,1,{non_specific,"?"}},
            block,sequence},
          {yamerl_doc_start,1,1,{1,2},_},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
