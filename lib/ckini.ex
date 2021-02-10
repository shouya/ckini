defmodule Ckini do
  alias Ckini.{Subst, Stream, Term}

  @type goal :: (Subst.t() -> Stream.t())

  @spec succ :: goal()
  def succ do
    fn s -> Stream.singleton(s) end
  end

  @spec fail :: goal()
  def fail do
    fn _ -> Stream.empty() end
  end

  @spec eq(Term.t(), Term.t()) :: goal()
  def eq(v, w) do
    fn s ->
      case unify(v, w, s) do
        :fail -> Stream.empty()
        new_s -> Stream.singleton(new_s)
      end
    end
  end

  @spec neq(Term.t(), Term.t()) :: goal()
  def neq(v, w) do
    fn s ->
      case unify(v, w, s) do
        :fail -> Stream.singleton(s)
        _s -> Stream.empty()
      end
    end
  end

  @spec unify(Term.t(), Term.t(), Subst.t()) :: :fail | Subst.t()
  defp unify(v, w, s) do
    vv = Subst.walk(s, v)
    ww = Subst.walk(s, w)

    cond do
      Term.eq?(vv, ww) -> s
      Term.var?(vv) -> Subst.insert(s, vv, ww)
      Term.var?(ww) -> Subst.insert(s, ww, vv)
      Term.list?(vv) and Term.list?(ww) -> unify_list(vv, ww, s)
      true -> :fail
    end
  end

  @spec unify_list([Term.t()], [Term.t()], Subst.t()) :: :fail | Subst.t()
  defp unify_list([], [], s), do: s

  defp unify_list([a | as], [b | bs], s) do
    case unify(a, b, s) do
      :fail -> :fail
      new_s -> unify(as, bs, new_s)
    end
  end

  defp unify_list(_, _, _s), do: :fail

  def run(vars, goals) do
    stream_of_subst = to_goal(goals).(Subst.new())

    stream_of_subst
    |> Stream.map(fn subst -> Term.reify(vars, subst) end)
    |> Stream.to_list()
  end

  def run(n, vars, goals) when is_integer(n) and n >= 0 do
    stream_of_subst = to_goal(goals).(Subst.new())

    stream_of_subst
    |> Stream.map(fn subst -> Term.reify(vars, subst) end)
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
    fn s ->
      goals = to_goal_stream(goals)
      Stream.bind_goals(Stream.singleton(s), goals)
    end
  end

  def conde(goals) do
    fn s ->
      goals
      |> to_goal_stream()
      |> Stream.map(fn g -> g.(s) end)
      |> Stream.concat()
    end
  end

  def condi(goals) do
    fn s ->
      goals
      |> to_goal_stream()
      |> Stream.map(fn g -> g.(s) end)
      |> Stream.interleave()
    end
  end

  @doc """
  conda is similar to conde but returns the first successful match of
  its subgoals.

  iex> x = Ckini.Var.new()
  iex> run(x, conda([[eq(x, :olive)], [eq(x, :oil)]]))
  [:olive]

  On the other hand, with conde all goals will be traversed.

  iex> x = Ckini.Var.new()
  iex> run(x, conde([eq(x, :olive), eq(x, :oil)]))
  [:olive, :oil]

  Note that conda perform "soft-cut" operation on its subgoals. In the
  following example, since x already successfully unifies with :olive,
  it will act as if there is no eq(x, :oil) branch.

  iex> x = Ckini.Var.new()
  iex> run(x, conda([[eq(x, :olive), eq(false, true)], [eq(x, :oil)]]))
  []
  """
  def conda(goals) do
    fn s ->
      {subs, subgoals} =
        Enum.find_value(goals, fn [subgoal | subgoals] ->
          subs = to_goal(subgoal).(s)
          if not Stream.empty?(subs), do: {subs, subgoals}
        end)

      subgoals = to_goal_stream(subgoals)
      Stream.bind_goals(subs, subgoals)
    end
  end

  @doc """
  condu is almost the same as conda, except that it only succeed once.

  iex> import Ckini.Goals
  iex> x = Ckini.Var.new()
  iex> alwayso = anyo(succ())
  iex> run(x, [condu([[eq(1, 2)], [alwayso]]), eq(x, 3)])
  [3]

  Above example will run forever if we replace condu with conda.
  """
  def condu(goals) do
    fn s ->
      {subs, subgoals} =
        Enum.find_value(goals, fn [subgoal | subgoals] ->
          subs = to_goal(subgoal).(s)
          if not Stream.empty?(subs), do: {subs, subgoals}
        end)

      subgoals = to_goal_stream(subgoals)
      Stream.bind_goals(Stream.take(subs, 1), subgoals)
    end
  end

  def project(var, f) do
    fn s ->
      to_goal(f.(Subst.deep_walk(s, var))).(s)
    end
  end
end
