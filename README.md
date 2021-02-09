# Ckini

A MiniKanren-like logic programming framework in Elixir.

## Usage

``` elixir
  defmodule Demo do
    import Ckini
    alias Ckini.Var

    def readme_demo do
      x = Var.new()
      y = Var.new()
      z = Var.new()

      run({x, y, z}, [
        # simple goal
        eq(y, 1),
        # a conde goal
        conde([eq(y, 2), eq(z, 3), eq(x, 4)]),
        # you can create logic variable any time
        fn ->
          t = Var.new()
          eq(x, [y, z, t, "hello"])
        end
      ])
    end
  end

  test "demo in README should work correctly" do
    assert [{[1, 3, :_0, "hello"], 1, 3}] == Demo.readme_demo()
  end
```


API available: `run/2`, `run/3`, `===`, `conde`, `condi`, `all`, `project`. (Please note `fresh` is not required with this syntax).

Check out the https://github.com/shouya/ckini/blob/master/test/ckini_test.exs for more usage examples.

I'm still working on a Macro interface to mimic the original TRS miniKanren implementation.

More documentation in progress...

## Improvement from The Little Schemer (TRS)'s MiniKanren implementation

1. Ckini implementation doesn't use any Macro. So the `fresh` construct is not needed.
2. Ckini allows multiple values to be queried in `run`, as opposed to one
3. Ckini's `walk` implementation has been designed to avoid circular reference
