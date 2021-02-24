defmodule MacroTest do
  @moduledoc false

  use ExUnit.Case

  import Ckini.Macro
  import Ckini.Goals, only: [eq: 2]

  defmacro debug_macro(do: body) do
    body
    |> Macro.prewalk(&Macro.expand(&1, __ENV__))
    |> Macro.to_string()
    |> Code.format_string!()
    |> :erlang.iolist_to_binary()
    |> IO.puts()

    body
  end

  test "foo" do
    goal =
      debug_macro do
        run(x) do
          eq(x, 1)
        end
      end

    assert [1] == goal
  end
end
