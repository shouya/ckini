defmodule Ckini.Goals do
  @moduledoc """
  This module defines generic goals and combinators like anyo.
  """

  import Ckini

  @doc """
  Anyo runs the goal for indefinitely number of times.

  iex> import Ckini
  iex> x = Ckini.Var.new()
  iex> run(5, x, anyo(eq(x, 1)))
  [1, 1, 1, 1, 1]
  """
  def anyo(g) do
    conde([g, fn -> anyo(g) end])
  end

  @doc """
  onceo succeeds at most once.

  iex> import Ckini
  iex> x = Ckini.Var.new()
  iex> run(x, onceo(anyo(eq(x, 1))))
  [1]
  """
  def onceo(g) do
    condu([[g]])
  end
end
