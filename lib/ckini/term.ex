defmodule Ckini.Term do
  @moduledoc """
  A logic term. It's either a variable or a list or a ground term
  represented by an Elixir term.
  """

  alias Ckini.{Var, Subst, Context}

  @type t ::
          atom()
          | binary()
          | integer()
          | Var.t()
          | maybe_improper_list(t(), t())

  defguard is_symbol(t) when is_atom(t)

  defguard is_basic(t)
           when is_symbol(t) or is_binary(t) or is_integer(t)

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

  @doc "check if a term is concrete, i.e. the term doesn't contain any variable"
  @spec concrete?(t(), Subst.t()) :: boolean()
  def concrete?(t, subst) do
    concrete?(Subst.deep_walk(subst, t))
  end

  def concrete?(t) do
    case t do
      %Var{} -> false
      t when is_basic(t) -> true
      xs when is_list(xs) -> Enum.all?(xs, &concrete?/1)
    end
  end

  @doc "test if term t contains term u with given substitution"
  def contains?(t, u, subst) do
    case Subst.walk(subst, t) do
      [h | t] -> contains?(h, u, subst) or contains?(t, u, subst)
      t -> equal?(t, u, subst)
    end
  end

  @doc "check equality of two terms under given substitution"
  def equal?(t, u, subst) do
    case Subst.unify(subst, u, t) do
      nil -> false
      s -> Subst.empty?(s)
    end
  end

  @doc "Reify all variables in a term and its related constraints"
  @spec reify(any(), Context.t()) :: any() | {any(), keyword()}
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
    r = Subst.reify(Subst.new(), t)

    ctx = Context.purify(ctx, r)
    term = Subst.deep_walk(r, t)

    case Context.constraints(ctx) do
      [] -> term
      cs -> {term, cs}
    end
  end
end
