-module('ex_9.4_explicit_documents').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        43,
        true,
        [],
        0,
        44,
        8,
        1,
        false,
        7,
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
        [{impl_key,false,false,undefined,undefined,1,1}],
        false,
        false,
        _,
        [],
        0,
        14,
        13,
        undefined,
        undefined,
        _,
        false,
        [
          {yamerl_parsing_error,warning,
            "An implicit key must not span several lines",
            2,3,invalid_implicit_key,
            undefined,[]}
        ],
        [
          {yamerl_stream_end,7,4},
          {yamerl_doc_end,7,1},
          {yamerl_scalar,7,1,
            {yamerl_tag,7,1,{non_specific,"?"}},
            flow,plain,""},
          {yamerl_doc_start,5,1,{1,2},_},
          {yamerl_doc_end,4,1},
          {yamerl_collection_end,3,8,flow,mapping},
          {yamerl_scalar,3,5,
            {yamerl_tag,3,5,{non_specific,"?"}},
            flow,plain,"20"},
          {yamerl_mapping_value,3,3},
          {yamerl_scalar,2,3,
            {yamerl_tag,2,3,{non_specific,"?"}},
            flow,plain,"matches %"},
          {yamerl_mapping_key,2,3},
          {yamerl_collection_start,2,1,
            {yamerl_tag,2,1,{non_specific,"?"}},
            flow,mapping},
          {yamerl_doc_start,1,1,{1,2},_},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
