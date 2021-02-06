defmodule ExCkini do
  import Kernel, except: [===: 2]
  alias ExCkini.{Var, Subst, Stream, Term}

  @type goal :: (Subst.t() -> Stream.t())

  @spec succ :: goal()
  def succ do
    fn s -> Stream.singleton(s) end
  end

  @spec fail :: goal()
  def fail do
    fn _ -> Stream.empty() end
  end

  # @spec (===)(Term.t(), Term.t()) :: goal()
  def v === w do
    fn s ->
      case unify(v, w, s) do
        :fail -> Stream.empty()
        new_s -> Stream.singleton(new_s)
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
      new_s -> unify_list(as, bs, new_s)
    end
  end

  defp unify_list(_, _, _s), do: :fail

  def run(vars, goals) do
    stream_of_subst = all(goals).(Subst.new())

    stream_of_subst
    |> Stream.map(fn subst -> Term.reify(vars, subst) end)
    |> Stream.to_list()
  end

  def run(n, vars, goals) when is_integer(n) and n >= 0 do
    stream_of_subst = all(goals).(Subst.new())

    stream_of_subst
    |> Stream.map(fn subst -> Term.reify(vars, subst) end)
    |> Stream.take(n)
  end

  def to_goal(f) when is_function(f, 0) do
    case f.() do
      g when is_function(g, 1) -> g
      _ -> raise "Please return a goal in a block"
    end
  end

  def to_goal(g) when is_function(g, 1), do: g

  def to_goal(gs) when is_list(gs), do: all(gs)

  def all(goals) do
    goals = Enum.map(goals, &to_goal(&1))

    fn s ->
      Stream.bind_goals(Stream.singleton(s), goals)
    end
  end

  def conde(goals) do
    goals = Enum.map(goals, &to_goal(&1))

    fn s ->
      subs = Enum.map(goals, fn g -> g.(s) end)
      Stream.concat(subs)
    end
  end

  def condi(goals) do
    goals = Enum.map(goals, &to_goal(&1))

    fn s ->
      subs = Enum.map(goals, fn g -> g.(s) end)
      Stream.interleave(subs)
    end
  end

  def ckini do
    y = Var.new()

    run(
      y,
      [
        conde([
          y === 1,
          y === 2,
          fn ->
            z = Var.new()
            x = Var.new()

            [x, y, z] === [y, z, :hello]
          end
        ])
      ]
    )
  end
end
