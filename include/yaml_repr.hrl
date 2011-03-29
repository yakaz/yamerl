%% -------------------------------------------------------------------
%% Options.
%% -------------------------------------------------------------------

-type yaml_repr_option() :: {node_mods, [atom()]}
                          | {simple_structs, boolean()}.

%% -------------------------------------------------------------------
%% Representation state.
%% -------------------------------------------------------------------

-record(yaml_repr, {
    options              = []         :: [yaml_repr_option()],
    simple_structs       = true       :: boolean(),
    mods                 = []         :: [atom()],
    tags                 = []         :: [{tag_uri(), atom()}],
    docs                 = []         :: [yaml_document() |
                                          yaml_simple_document()],
    docs_count           = 0          :: non_neg_integer(),
    current_doc          = undefined  :: [yaml_partial_document() |
                                          yaml_partial_node() |
                                          undefined],
    current_node_is_leaf = false      :: boolean(),
    anchors              = dict:new() :: dict()
  }).
