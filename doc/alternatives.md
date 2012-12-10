# Alternatives to yamerl

## YAML parsers

* [yamler](https://github.com/goertzenator/yamler):
 * Based on libyaml, wrapped in a NIF
 * Support YAML 1.1
 * Faster than yamerl
 * Support Erlang atoms, however, single-quoted scalar are treated as atom, which breaks the YAML specifications
 * Don't support Erlang fun()

## JSON parsers

* `mochijson2`, provided by [Mochiweb](https://github.com/mochi/mochiweb):
 * A lot faster than yamerl
