defmodule ExCkini do
  alias ExCkini.{Var, Subst}


  @type goal :: (Subst.t() -> Subst.t())

  def succ do
    fn s -> s end
  end

  def fail do
    fn _ -> [] end
  end

  def v === w do
    fn s ->
      unify(v, w, s)
    end
  end

  defp unify(_v, _w, s) do
    s
  end

  defmacro run({:fn, _, [{:->, _, [vars, body]}]}) do
    var_assignments =
      vars
      |> Enum.map(fn {v, _, Elixir} -> {v, Var.new(v)} end)
      |> Enum.map(fn {v, var} -> quote(do: unquote(v) = unquote(var)) end)
    |> (fn assigns -> [:__block__, [], assigns] end).()

    goals =
      case body do
        [:__block__, [], gs] -> gs
        g -> [g]
      end

    quote do
      unquote(var_assignments)
      bind(unquote(goals), )
    end
  end

  defmacro fresh({:fn, _, [{:->, _, [vars, body]}]}) do
    var_assignments =
      vars
      |> Enum.map(fn {v, _, Elixir} -> {v, Var.new(v)} end)
      |> Enum.map(fn {v, var} -> quote(do: unquote(v) = unquote(var)) end)
      |> (fn assigns -> [:__block__, [], assigns] end).()

    goals =
      case body do
        [:__block__, [], gs] -> gs
        g -> [g]
      end

    quote do
      fn s ->
        unquote(var_assignments)
        Subst.bind_goals(s, unquote(goals))
      end
    end
  end

  def hello do
    run_all(fn x ->
      fresh(fn y ->
        x === [y, y, 1]
      end)
    end)
  end
end
