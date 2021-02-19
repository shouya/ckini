defmodule Ckini.Macro do
  @moduledoc """
  MiniKanren-like interface using Macro.

  Exports run/1, run/2, fresh/1, conde/1.
  """

  alias Ckini.{Stream, Context}

  defmacro fresh(vars, do: goals) do
    quote do
      unquote(generate_vars(vars))
      all(do: unquote(goals))
    end
  end

  defmacro run(n, vars, do: goals) do
    quote do
      unquote(generate_vars(vars))
      goal = all(do: unquote(goals))

      goal
      |> apply([Stream.new(Context.new())])
      |> Stream.map(&Term.reify(unquote(vars), &1))
      |> Stream.take(unquote(n))
      |> Stream.to_list()
    end
  end

  defmacro all(do: goals) do
    bind_goals(goals)
  end

  defmacro conde(do: cases) do
    clauses =
      for {:->, _, [vars, clause]} <- cases do
        quote do
          fn -> fresh(unquote(vars), do: unquote(clause)).(ctx) end
        end
      end

    quote do
      fn ctx ->
        Stream.mplus_many(unquote(clauses))
      end
    end
  end

  defp bind_goals({:__block__, _, []}) do
    quote do: &Stream.singleton/1
  end

  defp bind_goals({:__block__, metadata, [g | gs]}) do
    quote do
      fn ctx ->
        ctxs = unquote(g).(ctx)
        goal = unquote(bind_goals({:__block__, metadata, gs}))

        ctxs
        |> Stream.map(fn ctx -> goal.(ctx) end)
        |> Stream.mplus_many()
      end
    end
  end

  defp bind_goals(single) do
    bind_goals({:__block__, [], [single]})
  end

  defp generate_vars({:{}, _, vars}) do
    generate_vars(vars)
  end

  defp generate_vars(vars) when is_list(vars) do
    quote do
      (unquote_splicing(
         for {name, _, _} = var <- vars do
           quote do: unquote(var) = Ckini.Var.new(unquote(name))
         end
       ))
    end
  end

  defp generate_vars({:_, _, _}) do
    quote do
    end
  end

  defp generate_vars({name, _, _} = var) do
    quote do
      unquote(var) = Ckini.Var.new(unquote(name))
    end
  end
end
