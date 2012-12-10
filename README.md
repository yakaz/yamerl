# yamerl: YAML 1.2 parser in Erlang

YAML is a human friendly data serialization format. The specification for this language  and many examples are available from the [Official YAML web site](http://www.yaml.org/). You may also want to check the [YAML Wikipedia article](http://en.wikipedia.org/wiki/YAML).

**yamerl** is a pure [Erlang application](http://www.erlang.org/) which is able to parse [YAML 1.1](http://yaml.org/spec/1.1/) and [YAML 1.2](http://www.yaml.org/spec/1.2/spec.html) documents.

yamerl only depends on standard Erlang/OTP applications, no external dependency is required. It doesn't use native code either (neither port drivers nor NIFs).

## Installation

Autotools and `make(1)` are used to build the application. After cloning the Git repository and entering the working copy directory:
```bash
# Generate Autotools files.
autoreconf -vif

# Build the application.
./configure
make
sudo make install
```

The default installation path is your Erlang's distribution libraries directory (see `code:lib_dir()`).

## Getting started

Before using yamerl, the application must be started:
```erlang
application:start(yamerl).
```

Now, one can use the `yamerl_constr` module to parse and construct a list of documents from:
* an in-memory document (string or binary);
* a file;
* a stream.

Because a YAML input stream may contain multiple documents, `yamerl_constr` always returns a list of documents, even if the input stream only contains one.

### Parsing an in-memory document

```erlang
yamerl_constr:string("Hello World!").
```
```erlang
% List of documents; here, only one document.
[
 % Document #1; contains a single scalar.
 "Hello World!"
]
```

Here, the returned value is a list of documents containing one document. This document has a scalar as its sole node.

### Parsing a file

Considering the following YAML file:
```yaml
# applications.yaml
- application: kernel
  version:     2.15.3
  path:        /usr/local/lib/erlang/lib/kernel-2.15.3
- application: stdlib
  version:     1.18.3
  path:        /usr/local/lib/erlang/lib/stdlib-1.18.3
- application: sasl
  version:     2.2.1
  path:        /usr/local/lib/erlang/lib/sasl-2.2.1
```

```erlang
yamerl_constr:file("applications.yaml").
```
```erlang
% List of documents; again, only one document here.
[
 % List of mappings.
 [
  % Mapping, represented as a proplist: each entry has the form {Key, Value}.
  [
   {"application", "kernel"},
   {"version", "2.15.3"},
   {"path", "/usr/local/lib/erlang/lib/kernel-2.15.3"}
  ], [
   {"application", "stdlib"},
   {"version", "1.18.3"},
   {"path", "/usr/local/lib/erlang/lib/stdlib-1.18.3"}
  ], [
   {"application", "sasl"},
   {"version", "2.2.1"},
   {"path", "/usr/local/lib/erlang/lib/sasl-2.2.1"}
  ]
 ]
]
```

### Parsing a stream

The developer is responsible for reading the stream and provide the chunks to yamerl.

```erlang
% Initialize a new construction state. It takes a term describing the
% source; it may be any Erlang term.
Parser0 = yamerl_constr:new({file, "<stdin>"}),

% Read chunks and feed the parser. A new parser state is returned.
{continue, Parser1} = yamerl_constr:next_chunk(Parser0, Chunk1),
% ...
{continue, Parser2} = yamerl_constr:next_chunk(Parser1, Chunk2),

% When the stream ends, tell the parser it's the last chunk.
Documents = yamerl_constr:last_chunk(Parser2, Chunk3).
```

## Simple vs. full document structures

`yamerl_constr` comes with two built-in modes:
* It can output simple documents, eg. documents based on basic Erlang structures (strings, numbers, lists, proplists). This is the default mode.
* It can output detailed documents using records. These records carry more information such as line/column, tag URI, YAML node type, module used to construct it, etc.

If we use the following YAML document:
```yaml
# system.yaml
- os: FreeBSD
  version: 9.0-RELEASE-p3
```

Simple documents:
```erlang
yamerl_constr:file("system.yaml").
```
```erlang
% List of documents.
[
 % List of mappings.
 [
  % Mapping with two entries.
  [
   {"os", "FreeBSD"},
   {"version","9.0-RELEASE-p3"}
  ]
 ]
]
```

Full documents:
```erlang
yamerl_constr:file("system.yaml", [{simple_structs, false}]).
```
```erlang
% List of documents.
[
 % Document with a list as its root node.
 {yamerl_doc,
  {yamerl_seq, yamerl_node_seq, "tag:yaml.org,2002:seq", [{line, 2}, {column, 1}], [
   % Mapping #1.
   {yamerl_map, yamerl_node_map, "tag:yaml.org,2002:map", [{line, 2}, {column, 3}], [
    {
     % Mapping entry #1.
     {yamerl_str, yamerl_node_str, "tag:yaml.org,2002:str", [{line, 2}, {column, 3}], "os"},
     {yamerl_str, yamerl_node_str, "tag:yaml.org,2002:str", [{line, 2}, {column, 7}], "FreeBSD"}
    }, {
     % Mapping entry #2.
     {yamerl_str, yamerl_node_str, "tag:yaml.org,2002:str", [{line, 3}, {column, 3}], "version"},
     {yamerl_str, yamerl_node_str, "tag:yaml.org,2002:str", [{line, 3}, {column, 12}], "9.0-RELEASE-p3"}
    }
   ]}
  ],
  1}
 }
]
```

## Complete documentation

See the `doc` subdirectory for a complete user guide and reference manual.
