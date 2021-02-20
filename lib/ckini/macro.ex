defmodule Ckini.Macro do
  @moduledoc """
  MiniKanren-like interface using Macro.

  Exports run, fresh, conde, condi, conda, condu.
  """

  alias Ckini.{Stream, Context, Term}

  defmacro fresh(vars, do: goals) do
    quote do
      unquote_splicing(generate_vars(vars))
      all(do: unquote(goals))
    end
  end

  defmacro run(vars, do: goals) do
    quote do
      unquote_splicing(generate_vars(vars))
      goal = all(do: unquote(goals))

      goal
      |> apply([Context.new()])
      |> Stream.map(&Term.reify(unquote(vars), &1))
      |> Stream.to_list()
    end
  end

  defmacro run(n, vars, do: goals) do
    quote do
      unquote_splicing(generate_vars(vars))
      goal = all(do: unquote(goals))

      goal
      |> apply([Context.new()])
      |> Stream.map(&Term.reify(unquote(vars), &1))
      |> Stream.take(unquote(n))
      |> Stream.to_list()
    end
  end

  defmacro all(do: goals) do
    bind_goals(goals)
  end

  defmacro conde(do: [{:->, _, [[vars], clause]}]) do
    quote do
      fn ctx ->
        unquote_splicing(generate_vars(vars))
        all(do: unquote(clause)).(ctx)
      end
    end
  end

  defmacro conde(do: cases) do
    clauses = cond_clauses_to_goals(cases)

    quote do
      fn ctx ->
        Stream.concat(Stream.from_list(unquote(clauses)))
      end
    end
  end

  defmacro condi(do: [{:->, _, [[_vars], _clause]}] = single) do
    quote do
      conde(do: unquote(single))
    end
  end

  defmacro condi(do: cases) do
    clauses = cond_clauses_to_goals(cases)

    quote do
      fn ctx ->
        Stream.mplus_many(Stream.from_list(unquote(clauses)))
      end
    end
  end

  defmacro conda(do: cases) do
    cases = extract_cond_clauses(cases)

    clauses =
      for {vars, [goal | goals]} <- cases do
        lhs =
          quote do
            unquote_splicing(generate_vars(vars))

            {ctx, ctxs} =
              case Stream.split(unquote(goal).(ctx)) do
                nil -> {nil, nil}
                {ctx, ctxs} -> {ctx, ctxs}
              end

            not is_nil(ctx)
          end

        ext_g =
          quote do
            fn _ -> Stream.cons(ctx, ctxs) end
          end

        rhs =
          quote do
            all(do: unquote(to_block([ext_g | goals]))).(ctx)
          end

        {:->, [], [[lhs], rhs]}
      end

    clauses = clauses ++ [{:->, [], [[true], quote(do: Stream.empty())]}]
    cond_expr = {:cond, [], [[do: clauses]]}

    quote location: :keep do
      fn ctx ->
        fn ->
          unquote(cond_expr)
        end
      end
    end
  end

  defmacro condu(do: cases) do
    cases = extract_cond_clauses(cases)

    clauses =
      for {vars, [goal | goals]} <- cases do
        lhs =
          quote do
            unquote_splicing(generate_vars(vars))
            res = Stream.to_list(Stream.take(unquote(goal).(ctx), 1))
            length(res) > 0
          end

        rhs =
          quote do
            [ctx] = res
            all(do: unquote(to_block(goals))).(ctx)
          end

        {:->, [], [[lhs], rhs]}
      end

    clauses = clauses ++ [{:->, [], [[true], quote(do: Stream.empty())]}]
    cond_expr = {:cond, [], [[do: clauses]]}

    quote location: :keep do
      fn ctx ->
        fn ->
          unquote(cond_expr)
        end
      end
    end
  end

  defp bind_goals({:__block__, _, []}) do
    quote do: &Stream.singleton/1
  end

  defp bind_goals({:__block__, _, [g]}), do: g

  defp bind_goals({:__block__, metadata, [g | gs]}) do
    quote do
      fn ctx ->
        fn ->
          ctxs = unquote(g).(ctx)
          goal = unquote(bind_goals({:__block__, metadata, gs}))

          ctxs
          |> Stream.map(fn ctx -> goal.(ctx) end)
          |> Stream.mplus_many()
        end
      end
    end
  end

  defp bind_goals(single) do
    bind_goals({:__block__, [], [single]})
  end

  defp generate_vars(vars) when is_list(vars) do
    for {name, _, _} = var when is_atom(name) <- vars do
      quote do: unquote(var) = Ckini.Var.new(unquote(name))
    end
  end

  defp generate_vars({{_, ctx, _} = var1, var2}) do
    generate_vars({:{}, ctx, [var1, var2]})
  end

  defp generate_vars({:{}, _, vars}) do
    generate_vars(vars)
  end

  defp generate_vars({:_, _, _}) do
    []
  end

  defp generate_vars({name, _, _} = var) when is_atom(name) do
    generate_vars([var])
  end

  defp cond_clauses_to_goals(cases) do
    for {vars, goals} <- extract_cond_clauses(cases) do
      quote do
        fn ->
          unquote_splicing(generate_vars(vars))
          all(do: unquote(to_block(goals))).(ctx)
        end
      end
    end
  end

  defp to_block(xs) do
    quote do
      (unquote_splicing(xs))
    end
  end

  defp extract_cond_clauses(cases) do
    for {:->, _, [[vars], clause]} <- cases do
      vars =
        case vars do
          [] -> []
          {:{}, _, []} -> []
          {:_, _, _} -> []
          {name, _, _} = var when is_atom(name) -> [var]
          vars when is_list(vars) -> vars
        end

      goals =
        case clause do
          {:__block__, _, []} -> []
          {:__block__, _, goals} -> goals
          goal -> [goal]
        end

      {vars, goals}
    end
  end
end
