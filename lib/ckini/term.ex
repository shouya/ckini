defmodule Ckini.Term do
  @moduledoc false

  alias Ckini.{Var, Subst}

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

  def reify(ts, ctx) when is_tuple(ts) do
    ts
    |> Tuple.to_list()
    |> reify(ctx)
    |> List.to_tuple()
  end

  def reify(t, ctx) do
    t = Subst.deep_walk(ctx.subst, t)
    Subst.deep_walk(Subst.reify(t), t)
  end
end
