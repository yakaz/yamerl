-ifndef(yamerl_errors_hrl).
-define(yamerl_errors_hrl, true).

-include("yamerl_types.hrl").
-include("yamerl_tokens.hrl").

%% -------------------------------------------------------------------
%% Errors and warnings.
%% -------------------------------------------------------------------

-record(yamerl_invalid_option, {
    type   = error :: error | warning,
    text           :: string() | undefined,

    option         :: term()
  }).

-record(yamerl_parsing_error, {
    type   = error :: error | warning,
    text           :: string() | undefined,

    line           :: position() | undefined,
    column         :: position() | undefined,
    name   = error :: atom(),
    token          :: yamerl_partial_token()
                    | undefined,
    extra  = []    :: [term()]
  }).

-type yamerl_error() :: #yamerl_invalid_option{}
                      | #yamerl_parsing_error{}.

-record(yamerl_exception, {
    errors = [] :: [yamerl_error()]
  }).
-type yamerl_exception() :: #yamerl_exception{}.

-endif.
