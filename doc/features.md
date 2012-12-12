# yamerl Features

## When to use or not yamerl

### Advantages

* Pure Erlang implementation:
  * should scale more easily than a port-driver based implementation;
  * won't take the whole VM down in case of a crash.
* YAML 1.2 support, which is not widely supported in many other languages.

### Caveats

* Current implementation is slow, compared to yamler (NIF-based) or any JSON-only parsers.
* Adding schemas is not easy.
* No support for YAML serialization.

## Features + examples

* Support [YAML 1.2](http://www.yaml.org/spec/1.2/spec.html) parsing:

  ```erlang
  yamerl_constr:string("YAML snippet").
  ```

* Support [YAML 1.1](http://yaml.org/spec/1.1/) parsing:

  ```erlang
  yamerl_constr:string("YAML snippet", [{schema, yaml11}]).
  ```

* Support [JSON](http://json.org/) parsing:

  ```erlang
  yamerl_constr:string(<<"JSON snippet">>, [{schema, json}]).
  ```

* Support **Erlang atom** node type, either when tagged as atom, or if autodetected in plain scalars, and, if asked, only if the atom already exists:

  ```erlang
  % Enable support for Erlang atoms.
  yamerl_app:set_param(node_mods, [yamerl_node_erlang_atom]),
  yamerl_constr:string("!<tag:yamerl,2012:atom> atom").

  % Autodetect Erlang atoms in plain scalars.
  yamerl_app:set_param(node_mods, [yamerl_node_erlang_atom]),
  yamerl_constr:string("atom", [{erlang_atom_autodetection, true}]).

  % Atoms must already exist.
  yamerl_app:set_param(node_mods, [yamerl_node_erlang_atom]),
  yamerl_constr:string("atom", [
      {erlang_atom_autodetection, true},
      {erlang_atom_only_if_exist, true}
  ]).
  ```

* Support **Erlang fun()** node type:

  ```erlang
  % Enable support for Erlang fun().
  yamerl_app:set_param(node_mods, [yamerl_node_erlang_fun]),
  [Plus_One_Fun] = yamerl_constr:string(<<"!<tag:yamerl,2012:fun> fun(X) -> X + 1 end.">>),

  Plus_One_Fun(2). % Return 3.
  ```

* Provide a **yamler compatibility layer**:

  ```erlang
  % Both calls return the same value.
  yaml:load_file("input.yaml", [{schema, yaml_schema_failsafe}]),
  yamerl_yamler_compat:load_file("input.yaml", [{schema, yaml_schema_failsafe}])
  ```

## Alternatives to yamerl

If yamerl doesn't fit your needs, you can read a [list of alternatives](alternatives.md#alternatives-to-yamerl).
