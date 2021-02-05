defmodule ExCkini.Stream do
  @moduledoc """
  This Subst module represents a stream of substitutions.
  """

  alias ExCkini.{Var, Term, Subst}

  defstruct [:car, :cdr]

  @type t :: %__MODULE__{} | nil
  @type subst :: Subst.t()

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

  @spec cached([subst], (() -> t())) :: t()
  defp cached([], f), do: f.()
  defp cached([x | xs], f), do: new(x, fn -> cached(xs, f) end)
end
