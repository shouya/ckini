defmodule ExCkini.Stream do
  @moduledoc """
  This module represents a stream data type.

  It's usually used to store a stream of substitutions but can also be mapped to
  other types.
  """

  defstruct [:car, :cdr]

  @type t :: nil | %__MODULE__{}
  @type goal :: ExCkini.goal()

  @spec new(any(), (() -> t())) :: t()
  def new(car, cdr) do
    %__MODULE__{car: car, cdr: cdr}
  end

  @spec empty :: t()
  def empty do
    nil
  end

  @spec singleton(any()) :: t()
  def singleton(s) do
    new(s, fn -> nil end)
  end

  @spec interleave(t(), t()) :: t()
  def interleave(subs1, nil), do: subs1
  def interleave(nil, subs2), do: subs2

  def interleave(%{car: x, cdr: xs}, subs2) do
    new(x, fn -> interleave(subs2, xs.()) end)
  end

  @spec insert(t(), t()) :: t()
  def insert(stream, v) do
    new(v, fn -> stream end)
  end

  # the input stream needs to be of element type Subst.t()
  @spec bind_goal(t(), goal()) :: t()
  def bind_goal(nil, _g), do: nil

  def bind_goal(%{car: x, cdr: xs} = c, g) do
    case g.(x) do
      nil ->
        bind_goal(xs.(), g)

      %{car: v, cdr: vs} ->
        new(v, fn -> cached(vs.(), fn -> bind_goal(xs.(), g) end) end)
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

  @spec map(t(), (any() -> any())) :: t()
  def map(nil, _f), do: nil

  def map(%{car: x, cdr: xs}, f) do
    new(f.(x), fn -> map(xs.(), f) end)
  end

  @spec take(t(), non_neg_integer()) :: [any()]
  def take(_s, 0), do: []
  def take(nil, _n), do: []

  def take(%{car: x, cdr: xs}, n) do
    [x | take(xs.(), n - 1)]
  end

  def to_list(nil), do: []
  def to_list(%{car: x, cdr: xs}), do: [x, to_list(xs.())]

  @spec cached(t(), (() -> t())) :: t()
  defp cached(nil, f), do: f.()

  defp cached(%{car: x, cdr: xs}, f),
    do: new(x, fn -> cached(xs.(), f) end)
end
