# Ckini

A miniKanren implementation in Elixir.

## Usage

``` elixir
  defmodule Demo do
    import Ckini
    alias Ckini.Var

    def readme_demo do
      run {x, y, z} do
        # simple goal
        eq(y, 1)
        # a conde goal
        conde do
          _ -> eq(y, 2)
          _ -> eq(z, 3)
          _ -> eq(x, 4)
        end
        # you can create logic variable with fresh
        fresh t do
          eq(x, [y, z, t, "hello"])
        end
      end
    end
  end

  test "demo in README should work correctly" do
    assert [{[1, 3, :_0, "hello"], 1, 3}] == Demo.readme_demo()
  end
```


API available: `run/2`, `run/3`, `eq` (`===`), `neq` (`=/=`), `cond{e,i,a,u}`, `match{e,i,a,u}`, `all`, `project`, `succ`, `fail`, `symbolo`, `absento`, `copy_termo`, `anyo`, `onceo`.

Check out the https://github.com/shouya/ckini/blob/master/test/*_test.exs for more usage examples.

## Feature highlights

- arithmetic operators (See `Ckini.Arithmetic`)
- introduction of fresh variables via `cond{e,i,a,u}`
- pattern matching with `match{e,i,a,u}` macros
- inequality (`neq`) constraint
- symbolo constraint
- absento constraint

## References

- The Reasoned Schemer by Daniel P. Friedman, et al.
- Relational Programming in miniKanren by William E. Byrd
