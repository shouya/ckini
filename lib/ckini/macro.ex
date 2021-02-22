defmodule Ckini.Macro do
  @moduledoc """
  MiniKanren-like interface using Macro.

  Exports run, fresh, conde, condi, conda, condu.
  """

  alias Ckini.{Stream, Context, Term}

  @doc """
  Fresh introduces new logic variables into the block scoped by do..end.

  Please note: with Ckini, you can minimize usage of `fresh` if you
  intend to use it immediately in a cond* and match* clause, as cond*
  and match* support introducing free variables via the left-hand-side
  of `->`.

  See `condi/1` and `matchi/2` for details.

  ## Example

  iex> use Ckini
  iex> run q do
  ...>   fresh x do
  ...>     eq([x, q], [q, 1])
  ...>   end
  ...> end
  [1]
  """
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
    bind_goals(extract_goals(goals))
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
            unquote(bind_goals_on_ctx([ext_g | goals], quote(do: ctx)))
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
            unquote(bind_goals_on_ctx(goals, quote(do: ctx)))
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

  defmacro matche(pattern, do: clauses),
    do: {:conde, [], [[do: match_to_cond_clause(pattern, clauses)]]}

  defmacro matchi(pattern, do: clauses),
    do: {:condi, [], [[do: match_to_cond_clause(pattern, clauses)]]}

  defmacro matcha(pattern, do: clauses),
    do: {:conda, [], [[do: match_to_cond_clause(pattern, clauses)]]}

  defmacro matchu(pattern, do: clauses),
    do: {:condu, [], [[do: match_to_cond_clause(pattern, clauses)]]}

  defp match_to_cond_clause(pattern, clauses) do
    pivot_pattern = extract_patterns(pattern)

    clauses =
      for {pats, vars, goals} <- extract_match_clauses(clauses) do
        quote location: :keep do
          unquote(vars) ->
            eq(unquote(pivot_pattern), unquote(pats))
            unquote_splicing(goals)
        end
      end

    Enum.map(clauses, &hd/1)
  end

  defp bind_goals([]) do
    quote do: &Stream.singleton/1
  end

  defp bind_goals([goal]), do: goal

  defp bind_goals(goals) do
    quote location: :keep do
      fn ctx ->
        unquote(bind_goals_on_ctx(goals, quote(do: ctx)))
      end
    end
  end

  defp bind_goals_on_ctx([], ctx) do
    quote do: Stream.singleton(unquote(ctx))
  end

  defp bind_goals_on_ctx([goal], ctx) do
    quote do: unquote(goal).(unquote(ctx))
  end

  defp bind_goals_on_ctx(goals, ctx) do
    quote location: :keep, bind_quoted: [goals: goals, ctx: ctx] do
      Enum.reduce(goals, Stream.singleton(ctx), fn goal, ctxs ->
        Stream.mplus_many(Stream.map(ctxs, &goal.(&1)))
      end)
    end
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
          unquote(bind_goals_on_ctx(goals, quote(do: ctx)))
        end
      end
    end
  end

  defp extract_match_clauses(cases) do
    for {:->, _, [lhs, clause]} <- cases do
      {patterns, vars} =
        case lhs do
          [patterns] -> {patterns, []}
          [patterns, vars] -> {patterns, vars}
        end

      patterns =
        patterns
        |> extract_patterns()
        |> Enum.map(&normalize_pattern/1)

      vars = extract_vars(vars) ++ all_vars_in_pattern(patterns)
      goals = extract_goals(clause)

      {patterns, vars, goals}
    end
  end

  defp all_vars_in_pattern(pattern) do
    case pattern do
      {:{}, _, xs} -> Enum.flat_map(xs, &all_vars_in_pattern/1)
      xs when is_list(xs) -> Enum.flat_map(xs, &all_vars_in_pattern/1)
      {name, _, _} = var when is_atom(name) -> [var]
      _ -> []
    end
  end

  def normalize_pattern(pattern) do
    case pattern do
      xs when is_list(xs) -> Enum.map(xs, &normalize_pattern/1)
      {:_, _, ctx} -> Macro.unique_var(:anon, ctx)
      {name, _, _} = var when is_atom(name) -> var
      val -> val
    end
  end

  defp extract_cond_clauses(cases) do
    for {:->, _, [vars, clause]} <- cases do
      vars = extract_vars(vars)
      goals = extract_goals(clause)
      {vars, goals}
    end
  end

  defp extract_vars(vars) do
    case vars do
      {:{}, _, vars} -> Enum.flat_map(vars, &extract_vars/1)
      vars when is_list(vars) -> Enum.flat_map(vars, &extract_vars/1)
      {var1, var2} -> [var1, var2]
      {:_, _, _} -> []
      {name, _, _} = var when is_atom(name) -> [var]
    end
  end

  defp extract_patterns(patterns) do
    case patterns do
      {:{}, _, pats} -> pats
      {pat1, pat2} -> [pat1, pat2]
      pats when is_list(pats) -> pats
      pat -> [pat]
    end
  end

  defp extract_goals(clause) do
    case clause do
      {:__block__, _, []} -> []
      {:__block__, _, goals} -> goals
      goal -> [goal]
    end
  end
end
