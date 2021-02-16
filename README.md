# Ckini

A miniKanren-like logic programming framework in Elixir.

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


API available: `run/2`, `run/3`, `eq` (`===`), `neq` (`=/=`), `conde`, `condi`, `conda`, `all`, `project`, `succ`, `fail`, `symbolo`, `copy_termo`, `anyo`, `onceo`.

(Please note `fresh` is not required with this syntax).

Check out the https://github.com/shouya/ckini/blob/master/test/ckini_test.exs for more usage examples.

I'm still working on a Macro interface to mimic the original TRS miniKanren implementation.

More documentation in progress...

## Features

1. Ckini's substitution is implemented using Elixir `Map` unify is implemented using incremental approach. This potentially makes it more efficient.
2. Ckini allows multiple values to be queried in `run`.
3. Ckini's implementation doesn't use any Macro. As a result, `fresh` is not needed.

## Roadmap

1. [x] conda and condu
2. [x] neq
3. [x] symbolo constraint
4. [x] absento constraint
5. [ ] numbero constraint

## References

- The Reasoned Schemer by Daniel P. Friedman, et al.
- Relational Programming in miniKanren by William E. Byrd
