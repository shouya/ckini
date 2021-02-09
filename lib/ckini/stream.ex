defmodule Ckini.Stream do
  @moduledoc """
  This module represents a stream data type.

  It's usually used to store a stream of substitutions but can also be mapped to
  other types.
  """

  defstruct [:car, :cdr]

  @type t(v) :: nil | %__MODULE__{car: v, cdr: (() -> t(v))}
  @type t :: t(any())
  @type goal :: Ckini.goal()
  # type var
  @typep a :: any()
  @typep b :: any()

  @spec cons(a(), (() -> t(a))) :: t(a)
  def cons(car, cdr) when is_function(cdr, 0) do
    %__MODULE__{car: car, cdr: cdr}
  end

  @spec empty :: t()
  def empty do
    nil
  end

  @spec singleton(a) :: t(a)
  def singleton(s) do
    cons(s, fn -> nil end)
  end

  @doc """
  Concat a stream of stream.

  iex> alias Ckini.Stream
  ...> [
  ...>   [0, 1, 2, 3],
  ...>   [4, 5, 6],
  ...>   [7],
  ...>   [8]
  ...> ]
  ...> |> Enum.map(&Stream.from_list/1)
  ...> |> Stream.from_list()
  ...> |> Stream.concat()
  ...> |> Stream.to_list()
  [0, 1, 2, 3, 4, 5, 6, 7, 8]
  """
  @spec concat(t(t(a))) :: t(a)
  def concat(nil), do: nil

  def concat(%{car: x, cdr: xs}) do
    case x do
      nil -> concat(xs.())
      %{car: y, cdr: ys} -> cons(y, fn -> concat(cons(ys.(), xs)) end)
    end
  end

  @doc """
  Like concat/1, but interleave a stream of stream instead.

  iex> alias Ckini.Stream
  ...> [
  ...>   [0, 4, 6, 8],
  ...>   [1, 5, 7],
  ...>   [2],
  ...>   [3]
  ...> ]
  ...> |> Enum.map(&Stream.from_list/1)
  ...> |> Stream.from_list()
  ...> |> Stream.interleave()
  ...> |> Stream.to_list()
  [0, 1, 2, 3, 4, 5, 6, 7, 8]
  """
  @spec interleave(t(t(a))) :: t(a)
  def interleave(nil), do: nil

  def interleave(%{car: x, cdr: xs}) do
    case x do
      nil -> interleave(xs.())
      %{car: y, cdr: ys} -> cons(y, fn -> interleave(snoc(xs, ys.())) end)
    end
  end

  @spec rotate1(t(a)) :: t(a)
  def rotate1(nil), do: nil
  def rotate1(%{car: x, cdr: xs}), do: snoc(xs, x)

  @spec snoc((() -> t(a)), a) :: t(a)
  def snoc(f, v) do
    case f.() do
      nil -> singleton(v)
      %{car: x, cdr: xs} -> cons(x, fn -> snoc(xs, v) end)
    end
  end

  # the input stream needs to be of element type Subst.t()
  @spec bind_goal(t(Subst.t()), goal()) :: t(Subst.t())
  def bind_goal(nil, _g), do: nil

  def bind_goal(%{car: x, cdr: xs}, g) do
    case g.(x) do
      nil ->
        bind_goal(xs.(), g)

      %{car: v, cdr: vs} ->
        cons(v, fn -> cached(vs.(), fn -> bind_goal(xs.(), g) end) end)
    end
  end

  @spec bind_goals(t(Subst.t()), t(goal)) :: t(Subst.t())
  def bind_goals(nil, _gs), do: nil
  def bind_goals(t, nil), do: t

  def bind_goals(t, %{car: g, cdr: gs}) do
    t |> bind_goal(g) |> bind_goals(gs.())
  end

  @spec map(t(a), (a -> b)) :: t(b)
  def map(nil, _f), do: nil

  def map(%{car: x, cdr: xs}, f) do
    cons(f.(x), fn -> map(xs.(), f) end)
  end

  @spec take(t(a) | (() -> t(a)), non_neg_integer()) :: t(a)
  def take(_s, 0), do: nil
  def take(nil, _n), do: nil

  def take(%{car: x, cdr: xs}, n) do
    cons(x, fn -> take(xs, n - 1) end)
  end

  def take(s, n) when is_function(s, 0), do: take(s.(), n)

  def to_list(nil), do: []
  def to_list(%{car: x, cdr: xs}), do: [x | to_list(xs.())]

  def from_list([]), do: nil
  def from_list([x | xs]), do: cons(x, fn -> from_list(xs) end)

  @spec filter(t(a), (a -> boolean())) :: t(a)
  def filter(nil, _pred), do: nil

  def filter(%{car: x, cdr: xs}, pred) do
    if pred.(x),
      do: cons(x, fn -> filter(xs.(), pred) end),
      else: filter(xs.(), pred)
  end

  @spec empty?(t()) :: boolean()
  def empty?(nil), do: true
  def empty?(%{car: _, cdr: _}), do: false

  @spec cached(t(a), (() -> t(a))) :: t(a)
  defp cached(nil, f), do: f.()

  defp cached(%{car: x, cdr: xs}, f),
    do: cons(x, fn -> cached(xs.(), f) end)
end
