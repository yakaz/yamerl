# yamerl Recipe: Basic parsing

1. Start the yamerl application. This is a mandatory step.

  ```erlang
  application:start(yamerl).
  ```

2. You're now ready to parse a serialized document:

  * To parse an in-memory string or binary:

   ```erlang
   Documents = yamerl_constr:string("Hello!").
   % Documents is a list of constructed documents.
   ```

  * To parse a file:

    ```erlang
    Documents = yamerl_constr:file("input.yaml").
    % Documents is a list of constructed documents.
    ```

  * To parse a stream:

    ```erlang
    % Create a new construction state. The only required argument is an
    % arbitrary term describing the source of the data. Here, we use the
    % same term structure as yamerl_constr:file/{1, 2}.
    Constr_State = yamerl_constr:new({file, "<stdin>"}),

    % Feed the parser with binary chunks. The developer is responsible for
    % reading the chunk from the underlying source.
    %
    % The function returns an updated construction state, which replace the
    % previous one.
    %
    % yamerl_constr:next_chunk/2 can be called as many times as possible.
    {continue, Constr_State2} = yamerl_constr:next_chunk(Constr_State, Chunk),

    % When the last chunk is reached, call yamerl_constr:last_chunk/2.
    Documents = yamerl_constr:last_chunk(Constr_State2, Last_Chunk).
    % Documents is a list of constructed documents.
    ```

> For further informations, see:
> * [yamerl\_constr module reference](../reference-manual/module-yamerl_constr.md);
