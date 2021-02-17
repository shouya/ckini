defmodule Ckini.Stream do
  @moduledoc """
  This module represents a stream data type.

  It's usually used to store a stream of substitutions but can also be mapped to
  other types.
  """

  defstruct [:car, :cdr]

  @type t(v) :: nil | %__MODULE__{car: v, cdr: (() -> t(v))} | (() -> t(v))
  @type t :: t(any())
  @type goal :: Ckini.goal()
  # type var
  @typep a :: any()
  @typep b :: any()

  defguard is_thunk(f) when is_function(f, 0)

  @spec new(a(), (() -> t(a))) :: t(a)
  def new(car, cdr) when is_function(cdr, 0) do
    %__MODULE__{car: car, cdr: cdr}
  end

  @spec cons(a(), (() -> t(a))) :: t(a)
  def cons(car, cdr) when is_function(cdr, 0) do
    fn -> new(car, cdr) end
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

  iex> [
  ...>   [0, 1, 2, 3],
  ...>   [4, 5, 6],
  ...>   [7],
  ...>   [8]
  ...> ]
  ...> |> Enum.map(&from_list/1)
  ...> |> from_list()
  ...> |> concat()
  ...> |> to_list()
  [0, 1, 2, 3, 4, 5, 6, 7, 8]
  """
  @spec concat(t(t(a))) :: t(a)
  def concat(nil), do: nil

  def concat(f) when is_thunk(f), do: fn -> concat(f.()) end

  def concat(%{car: x, cdr: xs}) do
    case x do
      nil -> concat(xs.())
      f when is_thunk(f) -> fn -> concat(new(f.(), xs)) end
      %{car: y, cdr: ys} -> cons(y, fn -> concat(cons(ys.(), xs)) end)
    end
  end

  @doc """
  Like concat/1, but interleave a stream of stream instead.

  iex> [
  ...>   [0, 4, 6, 8],
  ...>   [1, 5, 7],
  ...>   [2],
  ...>   [3]
  ...> ]
  ...> |> Enum.map(&from_list/1)
  ...> |> from_list()
  ...> |> interleave()
  ...> |> to_list()
  [0, 1, 2, 3, 4, 5, 6, 7, 8]
  """
  @spec interleave(t(t(a))) :: t(a)
  def interleave(nil), do: nil
  def interleave(f) when is_thunk(f), do: fn -> interleave(f.()) end

  def interleave(%{car: x, cdr: xs}) do
    case x do
      nil -> interleave(xs.())
      f when is_thunk(f) -> fn -> interleave(new(f.(), xs)) end
      %{car: y, cdr: ys} -> cons(y, fn -> interleave(snoc(xs, ys.())) end)
    end
  end

  @spec rotate1(t(a)) :: t(a)
  def rotate1(nil), do: nil
  def rotate1(f) when is_thunk(f), do: fn -> rotate1(f.()) end
  def rotate1(%{car: x, cdr: xs}), do: snoc(xs, x)

  @spec snoc((() -> t(a)), a) :: t(a)
  def snoc(f, v) do
    fn ->
      case f.() do
        nil -> singleton(v)
        f when is_thunk(f) -> fn -> snoc(f.(), v) end
        %{car: x, cdr: xs} -> cons(x, fn -> snoc(xs, v) end)
      end
    end
  end

  # the input stream needs to be of element type Subst.t()
  @spec bind_goal(t(Subst.t()), goal()) :: t(Subst.t())
  def bind_goal(nil, _g), do: nil
  def bind_goal(f, g) when is_thunk(f), do: fn -> bind_goal(f.(), g) end

  def bind_goal(%{car: x, cdr: xs}, g) do
    concat(cons(g.(x), cons(bind_goal(xs, g), fn -> nil end)))
  end

  @spec bind_goals(t(Subst.t()), t(goal)) :: t(Subst.t())
  def bind_goals(nil, _gs), do: nil
  def bind_goals(t, nil), do: t
  def bind_goals(f, g) when is_thunk(g), do: fn -> bind_goals(f, g.()) end

  def bind_goals(t, %{car: g, cdr: gs}) do
    t |> bind_goal(g) |> bind_goals(gs)
  end

  @spec map(t(a), (a -> b)) :: t(b)
  def map(nil, _f), do: nil
  def map(f, fun) when is_thunk(f), do: fn -> map(f.(), fun) end

  def map(%{car: x, cdr: xs}, f) do
    cons(f.(x), fn -> map(xs.(), f) end)
  end

  @spec take(t(a), non_neg_integer()) :: t(a)
  def take(_s, 0), do: nil
  def take(nil, _n), do: nil
  def take(f, n) when is_thunk(f), do: fn -> take(f.(), n) end

  # this clause is required so it doesn't try to resolve cdr if we
  # already have the one element we want.
  def take(%{car: x, cdr: _xs}, 1), do: singleton(x)

  def take(%{car: x, cdr: xs}, n) do
    cons(x, fn -> take(xs.(), n - 1) end)
  end

  def to_list(nil), do: []
  def to_list(f) when is_thunk(f), do: to_list(f.())
  def to_list(%{car: x, cdr: xs}), do: [x | to_list(xs.())]

  def from_list([]), do: nil
  def from_list([x | xs]), do: fn -> cons(x, fn -> from_list(xs) end) end

  @spec filter(t(a), (a -> boolean())) :: t(a)
  def filter(nil, _pred), do: nil
  def filter(f, pred) when is_thunk(f), do: fn -> filter(f.(), pred) end

  def filter(%{car: x, cdr: xs}, pred) do
    if pred.(x),
      do: cons(x, fn -> filter(xs.(), pred) end),
      else: filter(xs.(), pred)
  end

  @spec empty?(t()) :: boolean()
  def empty?(nil), do: true
  def empty?(f) when is_thunk(f), do: empty?(f.())
  def empty?(%{car: _, cdr: _}), do: false
end
