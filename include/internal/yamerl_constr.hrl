-ifndef(internal_yamerl_constr_hrl).
-define(internal_yamerl_constr_hrl, true).

-include("yamerl_nodes.hrl").

%% -------------------------------------------------------------------
%% Options.
%% -------------------------------------------------------------------

-type yamerl_constr_option() :: {detailed_constr, boolean()}
                              | {node_mods, [atom()]}
                              | {schema, failsafe | json | core | yaml11}.

%% -------------------------------------------------------------------
%% Representation state.
%% -------------------------------------------------------------------

-record(unfinished_node, {
    module = ?MODULE,
    path,
    pres,
    priv
  }).

-record(node_anchor, {
    name = ""
  }).

-record(yamerl_constr, {
    options              = []         :: [yamerl_constr_option()],
    ext_options          = []         :: [{term(), term()}],
    detailed_constr      = false      :: boolean(),
    mods                 = []         :: [atom()],
    tags                 = []         :: [{tag_uri(), atom()}],
    docs                 = []         :: [yamerl_doc() |
                                          yamerl_simple_doc()],
    docs_count           = 0          :: non_neg_integer(),
    current_doc          = undefined  :: [yamerl_partial_doc() |
                                          yamerl_partial_node() |
                                          #unfinished_node{} |
                                          #node_anchor{}]
                                       | undefined,
    current_node_is_leaf = false      :: boolean(),
    anchors              = dict:new() :: dict()
  }).

-endif.
