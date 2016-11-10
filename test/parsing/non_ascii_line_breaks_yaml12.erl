-module('non_ascii_line_breaks_yaml12').

-include_lib("eunit/include/eunit.hrl").

-define(FILENAME, "test/parsing/" ?MODULE_STRING ".yaml").

single_test_() ->
    ?_assertMatch(
      {yamerl_parser,
        {file,?FILENAME},
        [{io_blocksize, 1}],
        <<>>,
        62,
        true,
        [],
        0,
        58,
        4,
        1,
        false,
        3,
        46,
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
            "Use of non-ASCII line break is not supported anymore starting with YAML 1.2; treated as non-break character",
            3,45,non_ascii_line_break,
            undefined,[]},
          {yamerl_parsing_error,warning,
            "Use of non-ASCII line break is not supported anymore starting with YAML 1.2; treated as non-break character",
            3,25,non_ascii_line_break,
            undefined,[]},
          {yamerl_parsing_error,warning,
            "Use of non-ASCII line break is not supported anymore starting with YAML 1.2; treated as non-break character",
            3,10,non_ascii_line_break,
            undefined,[]}
        ],
        [
          {yamerl_stream_end,3,46},
          {yamerl_doc_end,3,46},
          {yamerl_scalar,3,1,
            {yamerl_tag,3,1,{non_specific,"?"}},
            flow,plain,
            [78,101,120,116,32,108,105,110,101,133,76,105,110,101,
              32,115,101,112,97,114,97,116,111,114,8232,80,97,114,
              97,103,114,97,112,104,32,115,101,112,97,114,97,116,
              111,114,8233]},
          {yamerl_doc_start,3,1,{1,2},_},
          {yamerl_yaml_directive,1,1,{1,2}},
          {yamerl_stream_start,1,1,utf8}
        ]
      },
      yamerl_parser:file(?FILENAME, [{io_blocksize, 1}])
    ).
