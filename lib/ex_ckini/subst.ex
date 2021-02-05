defmodule ExCkini.Subst do
  @moduledoc """
  A substitution is an ordered list of associations.
  An association is a {var, val} pair. See the type specs below for details.
  """

  alias ExCkini.{Var, Term}

  @type assoc :: {Var.t(), Term.t()}
  @type t :: [assoc()]

  @spec new(subst, (() -> t())) :: t()
  def new() do
    []
  end

  @spec insert(t(), subst()) :: t()
  def add(sub, var, val) do
    [{var, val} | sub]
  end
end
