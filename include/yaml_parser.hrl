%% -------------------------------------------------------------------
%% Supported versions range.
%% -------------------------------------------------------------------

-define(MIN_YAML_MAJOR_VERSION_SUPPORTED, 1).
-define(MIN_YAML_MINOR_VERSION_SUPPORTED, 1).
-define(MAX_YAML_MAJOR_VERSION_SUPPORTED, 1).
-define(MAX_YAML_MINOR_VERSION_SUPPORTED, 2).
-define(IMPLICIT_YAML_VERSION, {
    ?MAX_YAML_MAJOR_VERSION_SUPPORTED,
    ?MAX_YAML_MINOR_VERSION_SUPPORTED
  }).

%% -------------------------------------------------------------------
%% Options.
%% -------------------------------------------------------------------

-type yaml_parser_token_fun() :: fun((yaml_token()) -> ok | {ok, fun()}).

-type yaml_parser_option() :: {document_version, document_version()}
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
