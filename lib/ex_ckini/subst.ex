defmodule ExCkini.Subst do
  @moduledoc """
  A stream of {var, val}. Where {var, val} represents a single substitution.
  """


  defstruct [:car, :cdr]

  @type single_t :: {Var.t()}
  @type t :: %__MODULE__{}
  @type goal :: ExCkini.goal()

  def new(car, cdr) do
    %__MODULE__{car: car, cdr: cdr}
  end

  def empty do
    nil
  end

  def singleton(var, val) do
    new({var, val}, nil)
  end

  def interleave(subs1, nil), do: subs1
  def interleave(nil, subs2), do: subs2

  def interleave(%{car: x, cdr: xs}, subs2) do
    new(x, fn -> interleave(subs2, xs) end)
  end

  def insert(subs, var, val) do
    new({var, val}, subs)
  end

  @spec bind_goal(t(), goal()) :: t()
  def bind_goal(nil, _g), do: nil

  def bind_goal(%{car: x, cdr: xs}, g) do
    case f.(x) do
      nil ->
        bind_goal(xs, g)

      %{car: v, cdr: vs} ->
        new(v, fn -> cached(vs, fn -> bind_goal(xs, g) end) end)
    end
  end

  @spec bind_goals(t(), [goal()]) :: t()
  def bind_goals(nil, _gs), do: nil
  def bind_goals(subs, []), do: subs

  def bind_goals(subs, [g | gs]) do
    subs
    |> bind_goal(g)
    |> bind_goals(gs)
  end

  defp cached([], f), do: f
  defp cached([x | xs], f), do: new(x, fn -> cached(xs, f) end)
end
