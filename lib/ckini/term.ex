defmodule Ckini.Term do
  @moduledoc false

  alias Ckini.{Var, Subst}

  @type t :: atom() | binary() | integer() | Var.t() | [t()]

  def eq?(t1, t2) do
    cond do
      var?(t1) and var?(t2) -> Var.eq?(t1, t2)
      basic?(t1) and basic?(t2) -> t1 == t2
      list?(t1) and list?(t2) -> t1 == t2
      true -> false
    end
  end

  def basic?(t) do
    is_atom(t) or is_binary(t) or is_integer(t)
  end

  def var?(%Var{}), do: true
  def var?(_), do: false

  def list?([]), do: true
  def list?([_ | _]), do: true
  def list?(_), do: false

  def reify([], _subs), do: []

  def reify([t | ts], subs) do
    [reify(t, subs) | reify(ts, subs)]
  end

  def reify(ts, subs) when is_tuple(ts) do
    ts
    |> Tuple.to_list()
    |> Enum.map(&reify(&1, subs))
    |> List.to_tuple()
  end

  def reify(t, subs) do
    t = Subst.deep_walk(subs, t)
    Subst.deep_walk(Subst.reify(t), t)
  end
end
