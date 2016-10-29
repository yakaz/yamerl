# Recipe: Error handling

yamerl **throws an exception when an error occurs**.

1. Start the yamerl application. This is a mandatory step.

  ```erlang
  application:start(yamerl).
  ```

2. You're now ready to parse a serialized document:

  * To parse an in-memory string or binary:

    ```erlang
    -include_lib("yamerl/include/yamerl_errors.hrl").

    % ...

    try
      Documents = yamerl_constr:string("Hello!"),
      % Documents is a list of constructed documents.
      Documents
    catch
      throw:#yamerl_exception{errors = Errors} ->
        % Do something with the exception.
        Errors
    end.
    ```

As you can see, the `#yamerl_exception{}` record embeds all encountered errors:
```erlang
#yamerl_exception{
  errors = [] % List of errors.
}.
```

Errors are records where the two first members are always:

1. `type`, either `error` or `warning`;
2. `text`, a human-readable error message.

Following members depend on the error record. Two records are currently defined:
* `#yamerl_invalid_option{}`;
* `#yamerl_parsing_error{}`.

> For further informations, see:
> * [yamerl\_constr module reference](../reference-manual/module-yamerl_constr.md);
