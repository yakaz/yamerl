# Module: yamerl\_constr

## Functions list

* In-memory string parsing:
  * [`string/1`](#function-stringstring)
  * [`string/2`](#function-stringstring-options)
* File parsing:
  * [`file/1`](#function-filefilename)
  * [`file/2`](#function-filefilename-options)
* Stream parsing:
  * [`new/1`](#function-newsource)
  * [`new/2`](#function-newsource-options)
  * [`next_chunk/2`](#function-next_chunkconstr_state-chunk)
  * [`next_chunk/3`](#function-next_chunkconstr_state-chunk-last_chunk)
  * [`last_chunk/2`](#function-last_chunkconstr_state-chunk)

## Functions reference

### Function: new(Source)

Same as:
```erlang
yamerl_constr:new(Source, []).
```

> See [`new/2`](#function-newsource-options).

### Function: new(Source, Options)

#### Arguments

* `Source`: An arbitrary term describing the source of the data.
* `Options`: A proplist of options:
  * `{detailed_constr, boolean()}`: Flag to enable/disable the detailed construction mode (default: `false`).
  * `{schema, failsafe | json | core | yaml11}`: Name of the official schema to use (default: `core`).
  * `{node_mods, Mods_List}`: List of Erlang modules to extend supported node types (default: `[]`).

#### Description

Create a new construction state.

The returned state is then used in the [next\_chunk/2](#function-next_chunkconstr_state-chunk), [next\_chunk/3](#function-next_chunkconstr_state-chunk-last_chunk) and [last\_chunk/2](#function-last_chunkconstr_state-chunk) functions.

#### Return values & exceptions

If specified options are valid, return a new construction state.

If specified options are invalid, throw an exception.

### Function: next\_chunk(Constr\_State, Chunk)

Same as:
```erlang
yamerl_constr:next_chunk(Constr_State, Chunk, false).
```

> See [`next_chunk/3`](#function-next_chunkconstr_state-chunk-last_chunk).

### Function: next\_chunk(Constr\_State, Chunk, Last\_Chunk)

#### Arguments

* `Constr_State`: A construction state as returned by [new/1](#function-newsource), [new/2](#function-newsource-options), [next\_chunk/2](#function-next_chunkconstr_state-chunk) or [next\_chunk/3](#function-next_chunkconstr_state-chunk-last_chunk).
* `Chunk`: A UTF-8/16/32-encoded binary chunk of a stream containing one or more YAML documents.
* `Last_Chunk`: Flag indicating if the given chunk is the last one or not.

#### Description

Parse the given chunk and return a new construction state or, if last chunk, a list of constructed YAML documents.

The chunk must be an Erlang binary, using the UTF-8, UTF-16 or UTF-32 Unicode encoding. A leading _BOM_ character in the first chunk is used to determine the encoding and endianness. If no BOM is present, UTF-8 is assumed.

#### Return values & exceptions

If parsing succeeds and `Last_Chunk` is `false`, return `{continue, New_Constr_State}`. The `New_Constr_State` must be used for subsequent calls to [next\_chunk/2](#function-next_chunkconstr_state-chunk), [next\_chunk/3](#function-next_chunkconstr_state-chunk-last_chunk) or [last\_chunk/2](#function-last_chunkconstr_state-chunk).

If parsing and construction succeed and `Last_Chunk` is `true`, return a list of documents:
* Basic-terms-based documents are returned if option `{detailed_constr, false}` is set (default);
* Detailed records-based documents are returned if option `{detailed_constr, true}` is set.

If parsing or construction fails, throw an exception.

#### Examples

##### Parse a valid stream

```erlang
Stream_St1 = yamerl_constr:new({file, "<stdin>"}),
{continue, Stream_St2} = yamerl_constr:next_chunk(Stream_St1, <<"He">>),
{continue, Stream_St3} = yamerl_constr:next_chunk(Stream_St2, <<"ll">>),
yamerl_constr:last_chunk(Stream_St3, <<"o!">>).
```

Returns:
```erlang
% List of documents; here, only one.
[
  % Document root node: a string.
  "Hello!"
].
```

##### Parse an invalid stream

```erlang
Stream_St1 = yamerl_constr:new({file, "<stdin>"}),
{continue, Stream_St2} = yamerl_constr:next_chunk(Stream_St1, <<"'He">>),
{continue, Stream_St3} = yamerl_constr:next_chunk(Stream_St2, <<"ll">>),
yamerl_constr:last_chunk(Stream_St3, <<"o!">>) % Unfinished single-quoted scalar.
```

Throws:
```erlang
{yamerl_exception,
  % List of warnings and errors; here, one fatal error.
  [
    % Error #1.
    {yamerl_parsing_error, error,
      "Unexpected end-of-stream while parsing flow scalar",          % Human-readable message.
      1, 8,                                                          % Error location.
      unexpected_eos,
      {yamerl_scalar, 1, 1, {yamerl_tag, 1, 1, {non_specific, "!"}}, % Token being parsed.
        flow, single_quoted,
        "Hello!"},
      []
    }
  ]
}
```

### Function: last\_chunk(Constr\_State, Chunk)

Same as:
```erlang
yamerl_constr:next_chunk(Constr_State, Chunk, true).
```

> See [`next_chunk/3`](#function-next_chunkconstr_state-chunk-last_chunk).

### Function: string(String)

#### Description

Same as:
```erlang
yamerl_constr:string(String, []).
```

> See [`string/2`](#function-stringstring-options).

### Function: string(String, Options)

#### Arguments

* `String`: A string or UTF-8/16/32-encoded binary containing one or more YAML documents.
* `Options`: A proplist of options; see [`new/2`](#function-newsource-options).

#### Description

Parse the given string and return a list of constructed YAML documents.

The `String` argument can be:

* an Erlang string (ie. list):

  ```erlang
  yamerl_constr:string("This is a string").
  ```

* a binary using the UTF-8, UTF-16 or UTF-32 Unicode encoding. A leading _BOM_ character is used to determine the encoding and endianness. If no BOM is present, UTF-8 is assumed.

  ```erlang
  yamerl_constr:string(<<50,32,226,130,172>>). % The string "2 €" encoded in UTF-8.
  ```

#### Return values & exceptions

If parsing and construction succeed, return a list of documents:
* Basic-terms-based documents are returned if option `{detailed_constr, false}` is set (default);
* Detailed records-based documents are returned if option `{detailed_constr, true}` is set.

If parsing or construction fails, throw an exception.

#### Examples

##### Get simple documents

```erlang
yamerl_constr:string("Hello!").
```

Returns:
```erlang
% List of documents; here, only one.
[
  % Document root node: a string.
  "Hello!"
].
```

##### Get detailed documents

```erlang
yamerl_constr:string("Hello!", [detailed_constr]).
```

Returns:
```erlang
% List of documents; here, only one.
[
  % Document #1.
  {yamerl_doc,
    % Document root node: a string.
    {yamerl_str, yamerl_node_str, "tag:yaml.org,2002:str",
      [{line, 1}, {column, 1}], % Node location in the original string.
      "Hello!"                  % String value.
    }
  }
].
```

##### Parse an invalid document

```erlang
yamerl_constr:string(<<"'Oh-oh...">>). % Unfinished single-quoted scalar.
```

Throws an exception:
```erlang
{yamerl_exception,
  % List of warnings and errors; here, one fatal error.
  [
    % Error #1.
    {yamerl_parsing_error, error,
      "Unexpected end-of-stream while parsing flow scalar",          % Human-readable message.
      1, 10,                                                         % Error location.
      unexpected_eos,
      {yamerl_scalar, 1, 1, {yamerl_tag, 1, 1, {non_specific, "!"}}, % Token being parsed.
        flow, single_quoted,
        "Oh-oh..."},
      []
    }
  ]
}.
```

### Function: file(Filename)

#### Description

Same as:
```erlang
yamerl_constr:file(Filename, []).
```

> See [`file/2`](#function-filefilename-options).

### Function: file(Filename, Options)

#### Arguments

* `Filename`: A string containing the filename to parse.
* `Options`: A proplist of options; see [`new/2`](#function-newsource-options).

#### Description

Parse the file indicated by the given filename and return a list of constructed YAML documents.

The file is read by chunk of 4096 bytes (not configurable at this time).

Otherwise, the behavior is the same as [`string/2`](#function-stringstring-options).

> See [`string/2`](#function-stringstring-options) for return values, exceptions and examples.
