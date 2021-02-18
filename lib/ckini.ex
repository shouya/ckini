defmodule Ckini do
  alias Ckini.{Subst, Stream, Term, Context, Goals}

  @type goal :: (Context.t() -> Stream.t(Context.t()))

  defmacro __using__(_opts \\ []) do
    quote do
      import Ckini
      import Ckini.Goals
      alias Ckini.Var
    end
  end

  def run_s(vars, goals) do
    stream_of_subst = to_goal(goals).(Context.new())

    stream_of_subst
    |> Stream.map(fn subst -> Term.reify(vars, subst) end)
  end

  def run(vars, goals) do
    run_s(vars, goals)
    |> Stream.to_list()
  end

  def run(n, vars, goals) when is_integer(n) and n >= 0 do
    run_s(vars, goals)
    |> Stream.take(n)
    |> Stream.to_list()
  end

  defp to_goal(g) when is_function(g, 1), do: g
  defp to_goal(f) when is_function(f, 0), do: to_goal(f.())
  defp to_goal(gs) when is_list(gs), do: all(gs)
  defp to_goal(_), do: raise("Invalid goal!")

  defp to_goal_stream(gs) do
    gs
    |> Stream.from_list()
    |> Stream.map(fn g -> to_goal(g) end)
  end

  def all(goals) do
    fn ctx ->
      goals = to_goal_stream(goals)
      Stream.bind_goals(Stream.singleton(ctx), goals)
    end
  end

  def conde(goals) do
    fn ctx ->
      goals
      |> to_goal_stream()
      |> Stream.map(fn g -> g.(ctx) end)
      |> Stream.concat()
    end
  end

  def condem(goals) do
    fn ctx ->
      goals
      |> to_goal_stream()
      |> Stream.map(fn g -> g.(ctx) end)
      |> Stream.mplus_many()
    end
  end

  def condi(goals) do
    fn ctx ->
      goals
      |> to_goal_stream()
      |> Stream.map(fn g -> g.(ctx) end)
      |> Stream.interleave()
    end
  end

  @doc """
  conda is similar to conde but returns the first successful match of
  its subgoals.

  iex> use Ckini
  iex> x = Var.new()
  iex> run(x, conda([[eq(x, :olive)], [eq(x, :oil)]]))
  [:olive]

  On the other hand, with conde all goals will be traversed.

  iex> use Ckini
  iex> x = Var.new()
  iex> run(x, conde([eq(x, :olive), eq(x, :oil)]))
  [:olive, :oil]

  Note that conda perform "soft-cut" operation on its subgoals. In the
  following example, since x already successfully unifies with :olive,
  it will act as if there is no eq(x, :oil) branch.

  iex> use Ckini
  iex> x = Var.new()
  iex> run(x, conda([[eq(x, :olive), eq(false, true)], [eq(x, :oil)]]))
  []
  """
  def conda(goals) do
    fn ctx ->
      {subs, subgoals} =
        goals
        |> Enum.map(fn
          g when is_function(g, 0) -> g.()
          gs -> gs
        end)
        |> Enum.find_value(fn [subgoal | subgoals] ->
          subs = to_goal(subgoal).(ctx)
          if not Stream.empty?(subs), do: {subs, subgoals}
        end)

      subgoals = to_goal_stream(subgoals)
      Stream.bind_goals(subs, subgoals)
    end
  end

  @doc """
  condu is almost the same as conda, except that it only succeed once.

  iex> use Ckini
  iex> x = Var.new()
  iex> alwayso = anyo(succ())
  iex> run(x, [condu([[eq(1, 2)], [alwayso]]), eq(x, 3)])
  [3]

  Above example will run forever if we replace condu with conda.
  """
  def condu(goals) do
    fn ctx ->
      {subs, subgoals} =
        goals
        |> Enum.map(fn
          g when is_function(g, 0) -> g.()
          gs -> gs
        end)
        |> Enum.find_value(fn [subgoal | subgoals] ->
          subs = to_goal(subgoal).(ctx)
          if not Stream.empty?(subs), do: {subs, subgoals}
        end)

      subgoals = to_goal_stream(subgoals)
      Stream.bind_goals(Stream.take(subs, 1), subgoals)
    end
  end

  def project(var, f) do
    fn ctx ->
      to_goal(f.(Subst.deep_walk(ctx.subst, var))).(ctx)
    end
  end

  def peek(var) do
    project(var, fn v ->
      IO.inspect(v)
      Goals.succ()
    end)
  end
end
