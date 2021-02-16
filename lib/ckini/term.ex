defmodule Ckini.Term do
  @moduledoc false

  alias Ckini.{Var, Subst, Context}

  @type t :: atom() | binary() | integer() | Var.t() | [t()]

  defguard is_basic(t)
           when is_atom(t) or is_binary(t) or is_integer(t)

  def eq?(t1, t2) do
    cond do
      var?(t1) and var?(t2) -> Var.eq?(t1, t2)
      is_basic(t1) and is_basic(t2) -> t1 == t2
      list?(t1) and list?(t2) -> t1 == t2
      true -> false
    end
  end

  def var?(%Var{}), do: true
  def var?(_), do: false

  def list?([]), do: true
  def list?([_ | _]), do: true
  def list?(_), do: false

  def all_vars(t) when is_basic(t), do: []
  def all_vars(%Var{} = v), do: [v]
  def all_vars([]), do: []
  def all_vars([x | xs]), do: all_vars(x) ++ all_vars(xs)

  def reify(ts, ctx) when is_tuple(ts) do
    ts
    |> Tuple.to_list()
    |> reify(ctx)
    |> case do
      {xs, c} -> {List.to_tuple(xs), c}
      xs -> List.to_tuple(xs)
    end
  end

  def reify(t, ctx) do
    t = Subst.deep_walk(ctx.subst, t)
    cs = Context.purify(ctx, t)

    subst =
      Subst.new()
      |> Subst.reify(t)
      |> Subst.reify_substs(cs)

    term = Subst.deep_walk(subst, t)
    neq = Context.reify_neq(subst, cs)

    case neq do
      [] -> term
      c -> {term, c}
    end
  end
end
