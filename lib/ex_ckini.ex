defmodule ExCkini do
  import Kernel, except: [===: 2]
  alias ExCkini.{Var, Subst, Stream, Term}

  @type goal :: (Subst.t() -> Stream.t())

  @spec succ :: goal()
  def succ do
    fn s -> Stream.singleton(s) end
  end

  @spec succ :: goal()
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

  defmacro run({:fn, _, [{:->, _, [[var], body]}]}) do
    logic_var = to_logic_vars([var])
    var_assignments = to_var_assignments([var], [logic_var])

    goals =
      case body do
        [:__block__, [], gs] -> gs
        g -> [g]
      end

    quote location: :keep do
      unquote(var_assignments)
      goals = unquote(goals)

      Subst.new()
      |> Stream.singleton()
      |> Stream.bind_goals(goals)
      |> Stream.map(fn subst ->
        t = Subst.deep_walk(subst, unquote(Macro.escape(logic_var)))
        Term.reify(t)
      end)
    end
  end

  defmacro fresh({:fn, _, [{:->, _, [vars, body]}]}) do
    logic_vars = to_logic_vars(vars)
    var_assignments = to_var_assignments(vars, logic_vars)

    goals =
      case body do
        [:__block__, [], gs] -> gs
        g -> [g]
      end

    quote location: :keep do
      fn s ->
        unquote(var_assignments)
        Stream.bind_goals(Stream.singleton(s), unquote(goals))
      end
    end
  end

  defp to_logic_vars(vars) do
    for {var, _, _} <- vars do
      Var.new(var)
    end
  end

  def to_var_assignments(quoted_vars, logic_vars) do
    exprs =
      for {qv, lv} <- Enum.zip(quoted_vars, logic_vars) do
        quote do: unquote(qv) = unquote(Macro.escape(lv))
      end

    [:__block__, [], exprs]
  end

  def hello do
    run(fn x ->
      fresh(fn y ->
        x === [y, y, 1]
      end)
    end)
  end
end
