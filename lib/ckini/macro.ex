defmodule Ckini.Macro do
  @moduledoc """
  MiniKanren-like interface using Macro.

  Exports run/1, run/2, fresh/1, conde/1.
  """

  alias Ckini.{Stream, Var, Term}

  defmacro run(n, {:fn, _, [{:->, _, [[var], body]}]})
           when is_integer(n) and n >= 0 do
    [logic_var] = to_logic_vars([var])
    var_assignments = to_var_assignments([var], [logic_var])

    goals =
      case body do
        {:__block__, _, gs} -> Enum.map(gs, &Macro.expand(&1, __CALLER__))
        g -> [Macro.expand(g, __CALLER__)]
      end

    quote location: :keep do
      unquote_splicing(var_assignments)
      goals = unquote(goals)

      Subst.new()
      |> Stream.singleton()
      |> Stream.bind_goals(goals)
      |> Stream.map(fn subst ->
        Term.reify(unquote(Macro.escape(logic_var)), subst)
      end)
      |> Stream.take(unquote(n))
    end
  end

  defmacro run({:fn, _, [{:->, _, [[var], body]}]}) do
    [logic_var] = to_logic_vars([var])
    var_assignments = to_var_assignments([var], [logic_var])

    goals =
      case body do
        {:__block__, _, gs} -> Enum.map(gs, &Macro.expand(&1, __CALLER__))
        g -> [Macro.expand(g, __CALLER__)]
      end

    quote location: :keep do
      unquote_splicing(var_assignments)
      goals = unquote(goals)

      Subst.new()
      |> Stream.singleton()
      |> Stream.bind_goals(goals)
      |> Stream.map(fn subst ->
        Term.reify(unquote(Macro.escape(logic_var)), subst)
      end)
      |> Stream.to_list()
    end
  end

  defmacro fresh({:fn, _, [{:->, _, [vars, body]}]}) do
    logic_vars = to_logic_vars(vars)
    var_assignments = to_var_assignments(vars, logic_vars)

    goals =
      case body do
        {:__block__, _, gs} -> Enum.map(gs, &Macro.expand(&1, __CALLER__))
        g -> [Macro.expand(g, __CALLER__)]
      end

    quote location: :keep do
      fn s ->
        unquote_splicing(var_assignments)
        Stream.bind_goals(Stream.singleton(s), unquote(goals))
      end
    end
  end

  defp to_logic_vars(vars) do
    for {var, _, _} <- vars do
      Var.new(var)
    end
  end

  defp to_var_assignments(quoted_vars, logic_vars) do
    for {qv, lv} <- Enum.zip(quoted_vars, logic_vars) do
      quote do: unquote(qv) = unquote(Macro.escape(lv))
    end
  end
end
