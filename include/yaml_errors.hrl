-ifndef(yaml_errors_hrl).
-define(yaml_errors_hrl, true).

-include("yaml_types.hrl").
-include("yaml_tokens.hrl").

%% -------------------------------------------------------------------
%% Errors and warnings.
%% -------------------------------------------------------------------

-record(yaml_invalid_option, {
    type   = error :: error | warning,
    text           :: string() | undefined,

    option         :: term()
  }).

-record(yaml_parsing_error, {
    type   = error :: error | warning,
    text           :: string() | undefined,

    line           :: position() | undefined,
    column         :: position() | undefined,
    name   = error :: atom(),
    token          :: yaml_partial_token()
                    | undefined,
    extra  = []    :: [term()]
  }).

-type yaml_error() :: #yaml_invalid_option{}
                    | #yaml_parsing_error{}.

-record(yaml_exception, {
    errors = [] :: [yaml_error()]
  }).
-type yaml_exception() :: #yaml_exception{}.

%% -------------------------------------------------------------------
%% Macros to access common members of the error records.
%% -------------------------------------------------------------------

-define(ERROR_TYPE(E), element(#yaml_parsing_error.type, E)).
-define(ERROR_TEXT(E), element(#yaml_parsing_error.text, E)).

-endif.
