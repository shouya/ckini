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

  defmacro run({:fn, _, [{:->, _, [vars, body]}]}) do
    var_assignments = to_var_assignments(vars)

    goals =
      case body do
        [:__block__, [], gs] -> gs
        g -> [g]
      end

    quote do
      unquote(var_assignments)
      goals = unquote(goals)
      Stream.bind_goals(Subst.new(), goals)
    end
  end

  defmacro fresh({:fn, _, [{:->, _, [vars, body]}]}) do
    var_assignments = to_var_assignments(vars)

    goals =
      case body do
        [:__block__, [], gs] -> gs
        g -> [g]
      end

    quote do
      fn s ->
        unquote(var_assignments)
        Stream.bind_goals(s, unquote(goals))
      end
    end
  end

  defp all(gs, subst) do
  end

  defp to_var_assignments(vars) do
    vars
    |> Enum.map(fn {v, _, _} = vv ->
      var = Var.new(v)
      quote(do: unquote(vv) = unquote(Macro.escape(var)))
    end)
    |> (fn assigns -> [:__block__, [], assigns] end).()
  end

  def hello do
    run(fn x ->
      fresh(fn y ->
        x === [y, y, 1]
      end)
    end)
  end
end
