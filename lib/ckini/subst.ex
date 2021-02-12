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

  @spec insert(t(), Var.t(), Term.t()) :: t()
  def insert(subs, var, val) do
    [{var, val} | subs]
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
      Term.var?(vv) -> [{vv, ww}]
      Term.var?(ww) -> [{ww, vv}]
      Term.list?(vv) and Term.list?(ww) -> unify_list(s, vv, ww)
      true -> nil
    end
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
  @spec verify(t(), t()) :: nil | t()
  def verify(_base, []), do: []

  def verify(base, [{v, w} | sub]) do
    case unify(v, w, base) do
      nil -> nil
      extra_sub -> concat(extra_sub, verify(concat(extra_sub, base), sub))
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
      [t | ts] -> [deep_walk(sub, t) | deep_walk(sub, ts)]
      t -> t
    end
  end

  @spec reify(t(), Term.t()) :: t()
  def reify(subs \\ new(), t) do
    case walk(subs, t) do
      %Var{} = v ->
        insert(subs, v, reify_name(length(subs)))

      [t | ts] ->
        subs |> reify(t) |> reify(ts)

      _ ->
        subs
    end
  end

  defp remove(subst, var) do
    List.keydelete(subst, var, 0)
  end

  defp reify_name(n) do
    :"_#{n}"
  end
end
