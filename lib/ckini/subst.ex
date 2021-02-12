defmodule Ckini.Subst do
  @moduledoc """
  A substitution is an ordered list of associations.
  An association is a {var, val} pair. See the type specs below for details.
  """

  alias Ckini.{Var, Term}
  require Term

  @type assoc :: {Var.t(), Term.t()}
  @type t :: [assoc]

  @spec new() :: t()
  def new() do
    []
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
    [{var, val} | subs]
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
    [{var, val}]
  end

  @spec concat(t(), t()) :: t()
  def concat(s1, s2) do
    s1 ++ s2
  end

  @spec assocs(t()) :: [{Var.t(), Term.t()}]
  def assocs(t), do: t

  # returns only extra associations
  @spec unify(t(), Term.t(), Term.t()) :: nil | t()
  def unify(s, v, w) do
    vv = walk(s, v)
    ww = walk(s, w)

    cond do
      Term.eq?(vv, ww) -> []
      Term.var?(vv) -> insert(new(), vv, ww)
      Term.var?(ww) -> insert(new(), ww, vv)
      Term.list?(vv) and Term.list?(ww) -> unify_list(s, vv, ww)
      true -> nil
    end
  end

  def relevant_vars(_subs, []), do: []

  def relevant_vars(subs, [v | vs]) do
    new_vs = Term.all_vars(walk(subs, v)) -- [v]
    [v | relevant_vars(subs, new_vs ++ vs)]
  end

  @spec filter_vars(t(), [Var.t()]) :: t()
  def filter_vars(subs, vs) do
    Enum.filter(subs, fn {v, _} -> Enum.any?(vs, &Var.eq?(&1, v)) end)
  end

  @spec unify_list([Term.t()], [Term.t()], t()) :: nil | t()
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
  def verify(base, subst, acc \\ [])
  def verify(_base, [], acc), do: acc

  def verify(base, [{v, w} | sub], acc) do
    case unify(base, v, w) do
      [] -> nil
      nil -> verify(base, sub, acc)
      s -> verify(concat(s, base), sub, concat(s, acc))
    end
  end

  def walk(subst, %Var{} = var) do
    case List.keyfind(subst, var, 0) do
      nil -> var
      {_, %Var{} = t} -> walk(remove(subst, var), t)
      {_, t} -> t
    end
  end

  def walk(_subst, t), do: t

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
  def to_list([]), do: []
  def to_list([{v, t} | sub]), do: [[v | t] | to_list(sub)]

  @spec reify(t(), Term.t()) :: t()
  def reify(subst \\ new(), t) do
    case walk(subst, t) do
      %Var{} = v ->
        insert(subst, v, reify_name(length(subst)))

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
  def reify_subst(subst, []), do: subst

  def reify_subst(subst, [{v, t} | s]) do
    subst
    |> reify(v)
    |> reify(t)
    |> reify_subst(s)
  end

  defp remove(subst, var) do
    List.keydelete(subst, var, 0)
  end

  defp reify_name(n) do
    :"_#{n}"
  end
end
