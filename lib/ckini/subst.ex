defmodule Ckini.Subst do
  @moduledoc """
  A substitution is set of associations.
  An association is a {var, val} pair. See the type specs below for details.
  """

  alias Ckini.{Var, Term}
  require Term

  @typedoc "An association representing the fact that Var is associated with Term"
  @type assoc :: {Var.t(), Term.t()}

  @typedoc "A substitution"
  @type t :: map()

  defguard is_empty(s) when is_map(s) and map_size(s) == 0

  @spec new() :: t()
  def new() do
    %{}
  end

  @doc """
  Equivalent to ext-s-nocheck
  """
  @spec insert(t(), Var.t(), Term.t()) :: t() | nil
  def insert(subs, var, val) do
    if occurs?(subs, var, val),
      do: nil,
      else: insert_unsafe(subs, var, val)
  end

  @doc """
  Equivalent to ext-s. It does circular reference check before inserting
  an association pair to a substitution.
  """
  @spec insert_unsafe(t(), Var.t(), Term.t()) :: t() | nil
  def insert_unsafe(subs, var, val) do
    Map.put(subs, var, val)
  end

  def occurs?(subs, var, val) do
    case walk(subs, val) do
      %Var{} = var2 -> Var.eq?(var2, var)
      [v | vs] -> occurs?(subs, var, v) or occurs?(subs, var, vs)
      _ -> false
    end
  end

  @spec singleton(Var.t(), Term.t()) :: t()
  def singleton(var, val) do
    %{var => val}
  end

  @spec concat(t(), t()) :: t() | nil
  def concat(s1, s2) do
    Enum.reduce(
      s1,
      s2,
      fn
        _, nil -> nil
        {k, v}, acc -> insert(acc, k, v)
      end
    )
  end

  @spec assocs(t()) :: [{Var.t(), Term.t()}]
  def assocs(t), do: t

  # returns only extra associations
  @spec unify(t() | nil, Term.t(), Term.t()) :: nil | t()
  def unify(nil, _v, _w), do: nil

  def unify(s, v, w) do
    vv = walk(s, v)
    ww = walk(s, w)

    cond do
      Term.eq?(vv, ww) -> new()
      Term.var?(vv) -> insert(new(), vv, ww)
      Term.var?(ww) -> insert(new(), ww, vv)
      Term.list?(vv) and Term.list?(ww) -> unify_list(s, vv, ww)
      true -> nil
    end
  end

  @spec anyvar?(t(), Term.t()) :: boolean()
  def anyvar?(sub, t) do
    case t do
      %Var{} = t -> Term.var?(walk(sub, t))
      [h | t] -> anyvar?(sub, h) or anyvar?(sub, t)
      _ -> false
    end
  end

  @spec empty?(t()) :: boolean()
  def empty?(t) do
    is_empty(t)
  end

  @spec unify_list(t(), [Term.t()], [Term.t()]) :: nil | t()
  defp unify_list(_s, [], []), do: []

  defp unify_list(s, [a | as], [b | bs]) do
    case unify(s, a, b) do
      nil ->
        nil

      ss ->
        case unify(concat(ss, s), as, bs) do
          nil -> nil
          sss -> concat(ss, sss)
        end
    end
  end

  # list length incompatible
  defp unify_list(_s, _, _), do: nil

  @doc """
  Verify an neq substitution against a base substitution.
  """
  @spec verify_neq(t(), t(), t()) :: nil | t()
  def verify_neq(base, subst, acc \\ new())
  def verify_neq(_base, subst, acc) when is_empty(subst), do: acc

  def verify_neq(base, subst, acc) do
    {{v, w}, sub} = decompose(subst)

    case unify(base, v, w) do
      m when is_empty(m) -> nil
      nil -> verify_neq(base, sub, acc)
      s -> verify_neq(concat(s, base), sub, concat(s, acc))
    end
  end

  @spec decompose(t()) :: nil | {{Var.t(), Term.t()}, t()}
  def decompose(subst) do
    case Map.keys(subst) do
      [] ->
        nil

      [v | _] ->
        {t, s} = Map.pop!(subst, v)
        {{v, t}, s}
    end
  end

  @spec walk(t(), Term.t() | tuple()) :: Term.t() | tuple()
  def walk(subst, %Var{} = var) do
    case Map.get(subst, var) do
      nil -> var
      %Var{} = t -> walk(subst, t)
      t -> t
    end
  end

  def walk(_subst, t), do: t

  @spec deep_walk(t(), Term.t() | tuple()) :: Term.t() | tuple()
  def deep_walk(sub, var) do
    case walk(sub, var) do
      %Var{} = t -> t
      [] -> []
      [t | ts] -> [deep_walk(sub, t) | deep_walk(sub, ts)]
      t when is_tuple(t) -> List.to_tuple(deep_walk(sub, Tuple.to_list(t)))
      t -> t
    end
  end

  @doc "Convert a substitution to a valid list term"
  @spec to_list(t()) :: Term.t()
  def to_list(sub) do
    Enum.map(sub, fn {k, v} -> [k | v] end)
  end

  def to_pairs(sub) do
    Enum.to_list(sub)
  end

  @spec reify(t(), Term.t()) :: t()
  def reify(subst \\ new(), t) do
    case walk(subst, t) do
      %Var{} = v ->
        insert(subst, v, reify_name(size(subst)))

      [t | ts] ->
        subst |> reify(t) |> reify(ts)

      _ ->
        subst
    end
  end

  def reify_substs(subst, []), do: subst

  def reify_substs(subst, [c | cs]) do
    subst
    |> reify_subst(c)
    |> reify_substs(cs)
  end

  def reify_subst(subst \\ new(), sub)
  def reify_subst(subst, s) when is_empty(s), do: subst

  def reify_subst(subst, sub) do
    Enum.reduce(sub, subst, fn {v, t}, subst ->
      subst |> reify(v) |> reify(t)
    end)
  end

  # defp reify_name(0), do: :x
  # defp reify_name(1), do: :y
  # defp reify_name(2), do: :z

  defp reify_name(n) do
    :"_#{n}"
  end

  def size(s), do: map_size(s)
end
