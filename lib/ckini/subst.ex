defmodule Ckini.Subst do
  @moduledoc """
  A substitution is an ordered list of associations.
  An association is a {var, val} pair. See the type specs below for details.
  """

  alias Ckini.{Var, Term}

  @type assoc :: {Var.t(), Term.t()}
  @type t :: [assoc()]

  @spec new() :: t()
  def new() do
    []
  end

  @spec insert(t(), Var.t(), Term.t()) :: t()
  def insert(sub, var, val) do
    [{var, val} | sub]
  end

  def walk(subst, var) do
    case List.keyfind(subst, var, 0) do
      nil ->
        var

      {_, t} ->
        if Term.basic?(t) or Term.list?(t),
          do: t,
          else: walk(remove(subst, var), t)
    end
  end

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
