defmodule ExCkini.Stream do
  @moduledoc """
  This module represents a stream of substitutions.
  """

  alias ExCkini.{Subst}

  defstruct [:car, :cdr]

  @type t :: nil | %__MODULE__{}
  @type subst :: Subst.t()
  @type goal :: ExCkini.goal()

  @spec new(subst, (() -> t())) :: t()
  def new(car, cdr) do
    %__MODULE__{car: car, cdr: cdr}
  end

  @spec empty :: t()
  def empty do
    nil
  end

  @spec singleton(subst) :: t()
  def singleton(s) do
    new(s, nil)
  end

  @spec interleave(t(), t()) :: t()
  def interleave(subs1, nil), do: subs1
  def interleave(nil, subs2), do: subs2

  def interleave(%{car: x, cdr: xs}, subs2) do
    new(x, fn -> interleave(subs2, xs) end)
  end

  @spec insert(t(), subst()) :: t()
  def insert(subs, s) do
    new(s, subs)
  end

  @spec bind_goal(t(), goal()) :: t()
  def bind_goal(nil, _g), do: nil

  def bind_goal(%{car: x, cdr: xs}, g) do
    case g.(x) do
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

  @spec cached(t(), (() -> t())) :: t()
  defp cached(nil, f), do: f.()

  defp cached(%{car: x, cdr: xs}, f),
    do: new(x, fn -> cached(xs, f) end)
end
