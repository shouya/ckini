defmodule Ckini.Stream do
  @moduledoc """
  This module represents a stream data type.

  It's usually used to store a stream of substitutions but can also be mapped to
  other types.
  """

  defstruct [:car, :cdr]

  @type t :: nil | %__MODULE__{}
  @type goal :: Ckini.goal()

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

  @spec concat(t()) :: t()
  def concat(nil), do: nil

  def concat(%{car: x, cdr: xs}) do
    case x do
      nil -> concat(xs.())
      %{car: y, cdr: ys} -> new(y, fn -> concat(new(ys.(), xs)) end)
    end
  end

  # sort of like concat . zip, but no element will be dropped if the
  # streams are not of equal length.
  @spec interleave(t()) :: t()
  def interleave(nil), do: nil

  def interleave(%{car: x, cdr: xs}) do
    case x.() do
      nil -> interleave(xs.())
      %{car: y, cdr: ys} -> new(y, fn -> interleave(snoc(ys.(), xs)) end)
    end
  end

  def rotate1(nil), do: nil
  def rotate1(%{car: x, cdr: xs}), do: snoc(xs, x)

  def snoc(nil, v), do: singleton(v)

  def snoc(%{car: x, cdr: xs}, v) do
    new(x, fn -> snoc(xs.(), v) end)
  end

  @spec insert(t(), t()) :: t()
  def insert(stream, v) do
    new(v, fn -> stream end)
  end

  # the input stream needs to be of element type Subst.t()
  @spec bind_goal(t(), goal()) :: t()
  def bind_goal(nil, _g), do: nil

  def bind_goal(%{car: x, cdr: xs}, g) do
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
  def to_list(%{car: x, cdr: xs}), do: [x | to_list(xs.())]

  def from_list([]), do: nil
  def from_list([x | xs]), do: new(x, fn -> from_list(xs) end)

  @spec cached(t(), (() -> t())) :: t()
  defp cached(nil, f), do: f.()

  defp cached(%{car: x, cdr: xs}, f),
    do: new(x, fn -> cached(xs.(), f) end)
end
