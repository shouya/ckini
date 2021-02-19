defmodule MacroTest do
  @moduledoc false

  use ExUnit.Case

  import Ckini.Macro
  import Ckini.Goals

  defmacro debug_macro(do: body) do
    body
    |> Macro.prewalk(&Macro.expand(&1, __ENV__))
    |> Macro.to_string()
    |> IO.puts()

    body
  end

  test "foo" do
    goal =
      debug_macro do
        run([q]) do
          fresh({x}) do
            eq(q, x)
          end

          fresh({x}) do
            eq(x, 10)
          end
        end
      end

    assert 2 == goal
  end
end
