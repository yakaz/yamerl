-ifndef(yaml_repr_hrl).
-define(yaml_repr_hrl, true).

-include("yaml_nodes.hrl").

%% -------------------------------------------------------------------
%% Options.
%% -------------------------------------------------------------------

-type yaml_repr_option() :: {simple_structs, boolean()}.

%% -------------------------------------------------------------------
%% Representation state.
%% -------------------------------------------------------------------

-record(unfinished_node, {
    module = ?MODULE,
    path,
    pres,
    priv
  }).

-record(yaml_repr, {
    options              = []         :: [yaml_repr_option()],
    simple_structs       = true       :: boolean(),
    mods                 = []         :: [atom()],
    tags                 = []         :: [{tag_uri(), atom()}],
    docs                 = []         :: [yaml_doc() |
                                          yaml_simple_doc()],
    docs_count           = 0          :: non_neg_integer(),
    current_doc          = undefined  :: [yaml_partial_doc() |
                                          yaml_partial_node() |
                                          undefined],
    current_node_is_leaf = false      :: boolean(),
    anchors              = dict:new() :: dict()
  }).

-endif.
