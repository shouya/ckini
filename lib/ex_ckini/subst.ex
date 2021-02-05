defmodule ExCkini.Subst do
  @moduledoc """
  A substitution is an ordered list of associations.
  An association is a {var, val} pair. See the type specs below for details.
  """

  alias ExCkini.{Var, Term}

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

  end

  defp assoc(subst, var) do
    case Enum.find(subst, fn {v, t} -> if Var.eq?(var, v) end), do: t do
      nil -> nil
      {_,v} -> v
    end
  end
end
