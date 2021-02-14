defmodule Ckini.Subst do
  @moduledoc """
  A substitution is an ordered list of associations.
  An association is a {var, val} pair. See the type specs below for details.
  """

  alias Ckini.{Var, Term}
  require Term

  @type assoc :: {Var.t(), Term.t()}
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

  @spec concat(t(), t()) :: t()
  def concat(s1, s2) do
    Map.merge(s1, s2)
  end

  @spec assocs(t()) :: [{Var.t(), Term.t()}]
  def assocs(t), do: t

  # returns only extra associations
  @spec unify(t(), Term.t(), Term.t()) :: nil | t()
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

  @spec relevant_vars(t(), [Var.t()]) :: [Var.t()]
  def relevant_vars(_subs, []), do: []

  def relevant_vars(subs, [v | vs]) do
    new_vs = Term.all_vars(walk(subs, v)) -- [v]
    [v | relevant_vars(subs, new_vs ++ vs)]
  end

  @spec filter_vars(t(), [Var.t()]) :: t()
  def filter_vars(subs, vs) do
    Enum.filter(
      subs,
      fn {v, _} -> Enum.any?(vs, &Var.eq?(&1, v)) end
    )
    |> Map.new()
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
  Verify a substitution against a base substitution.
  """
  @spec verify(t(), t(), t()) :: nil | t()
  def verify(base, subst, acc \\ new())
  def verify(_base, subst, acc) when is_empty(subst), do: acc

  def verify(base, subst, acc) do
    {{v, w}, sub} = decompose(subst)

    case unify(base, v, w) do
      m when is_empty(m) -> nil
      nil -> verify(base, sub, acc)
      s -> verify(concat(s, base), sub, concat(s, acc))
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

  @spec walk(t(), Term.t()) :: Term.t()
  def walk(subst, %Var{} = var) do
    case Map.get(subst, var) do
      nil -> var
      %Var{} = t -> walk(subst, t)
      t -> t
    end
  end

  def walk(_subst, t), do: t

  @spec deep_walk(t(), Term.t()) :: Term.t()
  def deep_walk(sub, var) do
    case walk(sub, var) do
      %Var{} = t -> t
      [] -> []
      [t | ts] -> [deep_walk(sub, t) | deep_walk(sub, ts)]
      # a quick hack to allow querying with a tuple
      t when is_tuple(t) -> List.to_tuple(deep_walk(sub, Tuple.to_list(t)))
      t -> t
    end
  end

  @doc "Convert a substitution to a valid list term"
  @spec to_list(t()) :: Term.t()
  def to_list(sub) do
    Enum.map(sub, fn {k, v} -> [k | v] end)
  end

  def contraint_repr(subs) do
    Enum.map(subs, &Enum.to_list/1)
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

  defp reify_name(n) do
    :"_#{n}"
  end

  def size(s), do: map_size(s)
end
