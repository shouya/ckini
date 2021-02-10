defmodule Ckini.Goals do
  @moduledoc """
  This module defines generic goals and combinators like anyo.
  """

  alias Ckini.{Subst, Var}
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

  @doc """

  copy_termo creates a copy of its first argument, replacing logic
  variables with new variables.

  iex> import Ckini
  iex> w = Ckini.Var.new()
  iex> x = Ckini.Var.new()
  iex> y = Ckini.Var.new()
  iex> z = Ckini.Var.new()
  iex> run({w, z}, [
  ...>   eq([:a, x, 5, y, x], w),
  ...>   copy_termo(w, z)
  ...> ])
  [{[:a, :_0, 5, :_1, :_0],
    [:a, :_2, 5, :_3, :_2]}]
  """
  def copy_termo(t1, t2) do
    project(t1, fn t ->
      subs = build_s(Subst.new(), t)
      eq(t2, Subst.deep_walk(subs, t))
    end)
  end

  defp build_s(subs, t) do
    case t do
      %Var{} = v -> Subst.insert(subs, v, Var.clone(v))
      [t | ts] -> subs |> build_s(t) |> build_s(ts)
      _ -> subs
    end
  end
end
