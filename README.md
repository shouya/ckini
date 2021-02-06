# Ckini

A MiniKanren-like embedded logic programming framework.

## Usage

``` elixir
defmodule Foo do
    import Ckini

    def logic do
        # logic variables needs to be declared manually
        x = Ckini.Var.new()
        y = Ckini.Var.new()
        z = Ckini.Var.new()

        Ckini.run({x, y, z}, [
            # simple goal
            y === 1,
            # a conde goal
            conde([y === 2, z === 3, x === 4]),
            # it can be a block that returns a goal
            fn ->
                t = Ckini.Var.new()
                x === [y, z, t, "hello"]
            end
        ])

        # => [{[1, 3, :"_.0", "hello"], 1, 3}]
    end
end
```



API available: `run/2`, `run/3`, `===`, `conde`, `condi`, `all`. (`fresh` is not required in this implementation).

I'm still working on a Macro interface to mimic the original TRS miniKanren implementation.

More documentation in progress...

## Improvement from The Little Schemer (TRS)'s MiniKanren implementation

1. Ckini implementation doesn't use any Macro. As a result, variable assignment needs to be done manually
2. Ckini allows multiple values to be queried in `run`, as opposed to one
3. Ckini's `walk` implementation has been designed to avoid circular reference
