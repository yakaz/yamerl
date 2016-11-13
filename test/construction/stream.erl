-module(stream).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    application:start(yamerl).

simple_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertMatch(
          ["Hello!"],
          begin
              Stream_St1 = yamerl_constr:new("<stdin>"),
              {continue, Stream_St2} = yamerl_constr:next_chunk(Stream_St1,
                <<"He">>),
              {continue, Stream_St3} = yamerl_constr:next_chunk(Stream_St2,
                <<"ll">>),
              yamerl_constr:last_chunk(Stream_St3, <<"o!">>)
          end
        )
      ]
    }.

detailed_test_() ->
    {setup,
      fun setup/0,
      [
        ?_assertThrow(
          {yamerl_exception,
            [
              {yamerl_parsing_error, error,
                "Unexpected end-of-stream while parsing flow scalar",
                1, 8,
                unexpected_eos,
                {yamerl_scalar, 1, 1, {yamerl_tag, 1, 1, {non_specific, "!"}},
                  flow, single_quoted,
                  "Hello!"},
                []
              }
            ]
          },
          begin
              Stream_St1 = yamerl_constr:new("<stdin>"),
              {continue, Stream_St2} = yamerl_constr:next_chunk(Stream_St1,
                <<"'He">>),
              {continue, Stream_St3} = yamerl_constr:next_chunk(Stream_St2,
                <<"ll">>),
              yamerl_constr:last_chunk(Stream_St3, <<"o!">>)
          end
        )
      ]
    }.
