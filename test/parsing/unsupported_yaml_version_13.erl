-module('unsupported_yaml_version_13').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [{io_blocksize, 1}],
        <<>>,
        19,
        true,
        [],
        0,
        20,
        3,
        1,
        false,
        2,
        9,
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
        7,
        6,
        undefined,
        undefined,
        _,
        false,
        [
          {yamerl_parsing_error,warning,
            "Version 1.3 not supported (maximum version 1.2); parsing may fail",
            2,1,version_not_supported,
            {yamerl_doc_start,2,1,{1,3},_},
            []}
        ],
        [
          {yamerl_stream_end,2,9},
          {yamerl_doc_end,2,9},
          {yamerl_scalar,2,1,
            {yamerl_tag,2,1,{non_specific,"?"}},
            flow,plain,"Document"},
          {yamerl_doc_start,2,1,{1,3},_},
          {yamerl_yaml_directive,1,1,{1,3}},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME, [{io_blocksize, 1}])
    ).
