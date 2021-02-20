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
        run(2, x) do
          conda do
            z ->
              eq(x, z)

            [x, y] ->
              eq(x, y)
              eq(2, x)
          end
        end
      end

    assert 2 == goal
  end
end
