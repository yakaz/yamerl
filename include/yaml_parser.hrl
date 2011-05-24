-ifndef(yaml_parser_hrl).
-define(yaml_parser_hrl, true).

-include("yaml_tokens.hrl").

%% -------------------------------------------------------------------
%% Options.
%% -------------------------------------------------------------------

-type yaml_parser_token_fun() :: fun((yaml_token()) -> ok | {ok, fun()}).

-type yaml_parser_option() :: {default_tags, [{tag_uri(), tag_prefix()}]}
                            | {document_version, document_version()}
                            | {io_blocksize, pos_integer()}
                            | {token_fun, yaml_parser_token_fun()}.

%% -------------------------------------------------------------------
%% Errors and warnings.
%% -------------------------------------------------------------------

-record(yaml_parser_error, {
    line           :: position() | undefined,
    column         :: position() | undefined,
    type   = error :: error | warning,
    name   = error :: atom(),
    text           :: string() | undefined,
    token          :: yaml_partial_token()
                    | undefined,
    extra  = []    :: [term()]
  }).

-endif.
