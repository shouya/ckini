defmodule Ckini.Macro do
  @moduledoc """
  MiniKanren-like interface using Macro.

  Exports run, fresh, conde, condi, conda, condu.
  """

  alias Ckini.{Stream, Context, Term}

  @doc """
  Fresh introduces new logic variables into the block scoped by do..end. You can
  put the variables in a tuple to introduce many variables at the same time.

  Please note: with Ckini, you can minimize usage of `fresh` if you
  intend to use it immediately in a cond* and match* clause, as cond*
  and match* support introducing free variables via the left-hand-side
  of `->`.

  See `condi/1` and `matchi/2` for details.

  ## Examples

  iex> use Ckini
  iex> run q do
  ...>   fresh x do
  ...>     eq([x, q], [q, 1])
  ...>   end
  ...> end
  [1]
  iex> run q do
  ...>   fresh {x, y} do
  ...>     eq([x, y, q], [y, 1, x])
  ...>   end
  ...> end
  [1]

  Also see `all/1`.
  """
  defmacro fresh(vars, do: goals) do
    quote do
      unquote_splicing(generate_vars(extract_vars(vars)))
      all(do: unquote(goals))
    end
  end

  @doc """
  Query for all possible values of a variable with a goal.

  You can also query multiple variables by putting them in a tuple. The variable
  will be declared automatically.

  You can also provide multiple goals by putting them in the do block.

  ## Examples

  iex> use Ckini
  iex> run q do
  ...> end
  [:_0]
  iex> run {x, y} do
  ...>   eq(x, 1)
  ...>   conde do
  ...>     _ -> eq(y, 1)
  ...>     _ -> eq(y, 2)
  ...>     _ -> eq(x, 2)
  ...>   end
  ...> end
  [{1, 1}, {1, 2}]

  Also see `run/3` for limited of possible values.
  """
  defmacro run(vars, do: goals) do
    all_vars = extract_vars(vars)

    quote do
      unquote_splicing(generate_vars(all_vars))
      goal = all(do: unquote(goals))

      goal
      |> apply([Context.new()])
      |> Stream.map(&Term.reify(unquote(vars), &1))
      |> Stream.to_list()
    end
  end

  @doc """
  Query for a number of possible values of a variable with a goal.

  You can also query multiple variables by putting them in a tuple. The variable
  will be declared automatically.

  You can also provide multiple goals by putting them in the do block.

  ## Examples

  iex> use Ckini
  iex> run(2, q) do
  ...>   conde do
  ...>     _ -> eq(q, 1)
  ...>     _ -> eq(q, 2)
  ...>     _ -> eq(q, 3)
  ...>     _ -> eq(q, 4)
  ...>   end
  ...> end
  [1, 2]

  Also see `run/2` that allows querying unlimited possible values.
  """
  defmacro run(n, vars, do: goals) do
    all_vars = extract_vars(vars)

    quote do
      unquote_splicing(generate_vars(all_vars))
      goal = all(do: unquote(goals))

      goal
      |> apply([Context.new()])
      |> Stream.map(&Term.reify(unquote(vars), &1))
      |> Stream.take(unquote(n))
      |> Stream.to_list()
    end
  end

  @doc """
  Create a goal from a sequence of goals. The goal only succeeds when all the
  subgoals succeed.

  `all/1` is functionally equivalent to `fresh/1` except that it doesn't
  introduce variables.

  ## Examples

  iex> use Ckini
  iex> run(x) do
  ...>   eq(x, 1)
  ...>   all do
  ...>     eq(x, 2)
  ...>     eq(x, 1)
  ...>   end
  ...> end
  []

  The preceding example is only for demonstration. The usage of
  `all/1` in the example is not necessary.
  """
  defmacro all(do: goals) do
    bind_goals(extract_goals(goals))
  end

  @doc """
  Create a goal from a sequence of subgoals by taking disjunction on them.
  In other words, this syntax creates multiple possibilities.

  Conde performs depth-first search. Which means, conde will explore
  all possibilities of the first subgoal before exploring the second
  subgoal, and so on.

  Each subgoal is represented by a `->` expression in the do block.

  The left-hand side of `->` can be used to introduce new
  variables. You can put a single variable, or multiple variables
  enclosed by `{}`, or if no new variable is needed, put an `_`.

  The right-hand side of `->` can a single goal or multiple goals. If
  multiple goals are supplied, they will be treated as if they are
  enclosed in a `all/1` goal.

  ## Examples

  iex> use Ckini
  iex> run(x) do
  ...>   conde do
  ...>     _ -> eq(x, 2)
  ...>     _ -> eq(x, 1)
  ...>   end
  ...> end
  [2, 1]

  Also see `condi/1`, `conda/1`, `condu/1`, `matche/2`.
  """
  defmacro conde(do: cases) do
    clauses = cond_clauses_to_goals(cases)

    quote do
      fn ctx ->
        Stream.concat(Stream.from_list(unquote(clauses)))
      end
    end
  end

  @doc """
  Create a goal from a sequence of subgoals by taking disjunction on them.
  In other words, this syntax creates multiple possibilities.

  Unlike `conde/1`, `condi/1` performs a different type of search
  that's different from depth-first search. It will explore both
  breadth and depth in a zig-zag order, making it more useful for
  many cases.

  The syntax is the same as `conde/1`. See `conde/1` for usage.

  ## Examples

  iex> use Ckini
  iex> run(x) do
  ...>   condi do
  ...>     _ -> eq(x, 2)
  ...>     _ -> eq(x, 1)
  ...>   end
  ...> end
  [2, 1]

  Also see `conde/1`, `conda/1`, `condu/1`, `matchi/2`.
  """
  defmacro condi(do: cases) do
    clauses = cond_clauses_to_goals(cases)

    quote do
      fn ctx ->
        Stream.mplus_many(Stream.from_list(unquote(clauses)))
      end
    end
  end

  @doc """
  `conda/1` performs what's called a "soft-cut" operation in
  Prolog. In each of its subgoals, if they are composed of multiple
  goals and the first goal succeeds. `conda/1` will behave like the subgoal
  is the only branch.

  The syntax is the same as `conde/1`. See `conde/1` for usage.

  ## Examples

  iex> use Ckini
  iex> teacupo = fn x ->
  ...>   conde do
  ...>     _ -> eq(x, :tea)
  ...>     _ -> eq(x, :cup)
  ...>   end
  ...> end
  iex> run({x, y}) do
  ...>   conda do
  ...>     _ ->
  ...>       teacupo.(x)
  ...>       eq(y, x)
  ...>     _ ->
  ...>       eq(x, 1)
  ...>       eq(y, x)
  ...>   end
  ...> end
  [{:tea, :tea}, {:cup, :cup}]

  iex> use Ckini
  iex> teacupo = fn x ->
  ...>   conde do
  ...>     _ -> eq(x, :tea)
  ...>     _ -> eq(x, :cup)
  ...>   end
  ...> end
  iex> run({x, y}) do
  ...>   conda do
  ...>     _ ->
  ...>       teacupo.(x)
  ...>       eq(1, 2)
  ...>     _ ->
  ...>       eq(x, 1)
  ...>       eq(y, x)
  ...>   end
  ...> end
  []
  """
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

  @doc """
  `condu/1` is similar to `conda/1` in the sense that it also treats
  the first successful subgoal as the only branch. Unlike `conda/1`,
  `condu/1` also restrict its subgoal to have only one possibilities.

  ## Examples

  iex> use Ckini
  iex> teacupo = fn x ->
  ...>   conde do
  ...>     _ -> eq(x, :tea)
  ...>     _ -> eq(x, :cup)
  ...>   end
  ...> end
  iex> run({x, y}) do
  ...>   conda do
  ...>     _ ->
  ...>       teacupo.(x)
  ...>       eq(y, x)
  ...>     _ ->
  ...>       eq(x, 1)
  ...>       eq(y, x)
  ...>   end
  ...> end
  [{:tea, :tea}]

  iex> use Ckini
  iex> teacupo = fn x ->
  ...>   conde do
  ...>     _ -> eq(x, :tea)
  ...>     _ -> eq(x, :cup)
  ...>   end
  ...> end
  iex> run({x, y}) do
  ...>   condu do
  ...>     _ ->
  ...>       teacupo.(x)
  ...>       eq(1, 2)
  ...>     _ ->
  ...>       eq(x, 1)
  ...>       eq(y, x)
  ...>   end
  ...> end
  []
  """
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
    pivot_pattern = extract_pattern(pattern)

    clauses =
      for {pat, vars, goals} <- extract_match_clauses(clauses) do
        quote location: :keep do
          {unquote_splicing(vars)} ->
            eq(unquote(pivot_pattern), unquote(pat))
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

  defp generate_vars(vars) do
    for {name, _, _} = var <- vars do
      quote do: unquote(var) = Ckini.Var.new(unquote(name))
    end
  end

  defp cond_clauses_to_goals({:->, _, _} = single) do
    cond_clauses_to_goals([single])
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
    for {:->, _, [[pattern | vars], clause]} <- cases do
      pattern =
        pattern
        |> extract_pattern()
        |> Enum.map(&normalize_pattern/1)

      vars =
        case vars do
          [] -> all_vars_in_pattern(pattern)
          [vars] -> extract_vars(vars) ++ all_vars_in_pattern(pattern)
          _ -> raise "Use {var1, var2, ...} to introduce multiple variables"
        end

      goals = extract_goals(clause)

      {pattern, vars, goals}
    end
  end

  defp all_vars_in_pattern(pattern) do
    case pattern do
      xs when is_list(xs) -> Enum.flat_map(xs, &all_vars_in_pattern/1)
      {name, _, _} = var when is_atom(name) -> [var]
      _ -> []
    end
  end

  @spec normalize_pattern(Macro.t()) :: Macro.t()
  defp normalize_pattern(pattern) do
    case pattern do
      xs when is_list(xs) -> Enum.map(xs, &normalize_pattern/1)
      # multiple :_ can appear in pattern but they are not the same
      {:_, _, ctx} -> Macro.unique_var(:anon, ctx)
      {name, _, _} = var when is_atom(name) -> var
      val -> val
    end
  end

  defp extract_cond_clauses(cases) do
    for {:->, _, [[vars], clause]} <- cases do
      vars = extract_vars(vars)
      goals = extract_goals(clause)
      {vars, goals}
    end
  end

  defp extract_vars(vars) do
    case vars do
      {:_, _, _} -> []
      {:{}, _, vars} -> vars
      {var1, var2} -> [var1, var2]
      {name, _, _} = var when is_atom(name) -> [var]
    end
  end

  defp extract_pattern(pattern) do
    pattern
  end

  defp extract_goals(clause) do
    case clause do
      {:__block__, _, goals} -> Enum.flat_map(goals, &extract_goals/1)
      goals when is_list(goals) -> Enum.flat_map(goals, &extract_goals/1)
      goal -> [goal]
    end
  end
end
