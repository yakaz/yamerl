-module('ex_5.14_invalid_escaped_characters').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        27,
        true,
        [],
        0,
        28,
        4,
        1,
        false,
        3,
        8,
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
        11,
        10,
        undefined,
        undefined,
        _,
        false,
        [
          {yamerl_parsing_error,warning,
            "Invalid escaped character",
            3,3,invalid_escaped_character,
            {yamerl_scalar,2,3,{yamerl_tag,2,3,{non_specific,"!"}},flow,double_quoted,"c "},
            []},
          {yamerl_parsing_error,warning,
            "Invalid escaped character",
            2,4,invalid_escaped_character,
            {yamerl_scalar,2,3,{yamerl_tag,2,3,{non_specific,"!"}},flow,double_quoted,""},
            []}
        ],
        [
          {yamerl_stream_end,3,8},
          {yamerl_doc_end,3,8},
          {yamerl_collection_end,3,8,block,mapping},
          {yamerl_scalar,2,3,
            {yamerl_tag,2,3,{non_specific,"!"}},
            flow,double_quoted,"c xq-"},
          {yamerl_mapping_value,1,12},
          {yamerl_scalar,1,1,
            {yamerl_tag,1,1,{non_specific,"?"}},
            flow,plain,"Bad escapes"},
          {yamerl_mapping_key,1,1},
          {yamerl_collection_start,1,1,
            {yamerl_tag,1,1,{non_specific,"?"}},
            block,mapping},
          {yamerl_doc_start,1,1,{1,2},_},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
