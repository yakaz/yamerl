-module('ex_5.2_invalid_bom').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [],
        <<>>,
        90,
        true,
        [],
        0,
        45,
        4,
        1,
        false,
        3,
        21,
        {utf16,big},
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
        11,
        10,
        undefined,
        undefined,
        _,
        false,
        [
          {yamerl_parsing_error,warning,
            _,
            2,1,bom_after_doc_start,undefined,[]}
        ],
        [
          {yamerl_stream_end,3,21},
          {yamerl_doc_end,3,21},
          {yamerl_collection_end,3,21,block,sequence},
          {yamerl_scalar,3,3,
            {yamerl_tag,3,3,{non_specific,"?"}},
            flow,plain,"Inside a document."},
          {yamerl_sequence_entry,3,1},
          {yamerl_scalar,1,3,
            {yamerl_tag,1,3,{non_specific,"?"}},
            flow,plain,"Invalid use of BOM"},
          {yamerl_sequence_entry,1,1},
          {yamerl_collection_start,1,1,
            {yamerl_tag,1,1,{non_specific,"?"}},
            block,sequence},
          {yamerl_doc_start,1,1,{1,2},_},
          {yamerl_stream_start,1,1,{utf16,big}}
        ]
      },
      yamerl_parser:file(?FILENAME)
    ).
