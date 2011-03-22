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

-type yaml_parser_option() :: {io_blocksize, pos_integer()}
                            | {document_version, document_version()}.

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
