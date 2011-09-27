-ifndef(internal_yaml_constr_hrl).
-define(internal_yaml_constr_hrl, true).

-include("yaml_nodes.hrl").

%% -------------------------------------------------------------------
%% Options.
%% -------------------------------------------------------------------

-type yaml_constr_option() :: {simple_structs, boolean()}.

%% -------------------------------------------------------------------
%% Representation state.
%% -------------------------------------------------------------------

-record(unfinished_node, {
    module = ?MODULE,
    path,
    pres,
    priv
  }).

-record(yaml_constr, {
    options              = []         :: [yaml_constr_option()],
    simple_structs       = true       :: boolean(),
    mods                 = []         :: [atom()],
    tags                 = []         :: [{tag_uri(), atom()}],
    docs                 = []         :: [yaml_doc() |
                                          yaml_simple_doc()],
    docs_count           = 0          :: non_neg_integer(),
    current_doc          = undefined  :: [yaml_partial_doc() |
                                          yaml_partial_node()]
                                       | undefined,
    current_node_is_leaf = false      :: boolean(),
    anchors              = dict:new() :: dict()
  }).

-endif.
