defmodule Ckini.Stream do
  @moduledoc """
  This module represents a stream data type.

  It's usually used to store a stream of substitutions but can also be mapped to
  other types.
  """

  alias Ckini.Context

  defstruct [:car, :cdr]

  @type t(v) :: nil | %__MODULE__{car: v, cdr: t(v)} | (() -> t(v))
  @type t :: t(any())

  @type goal :: (Context.t() -> t(Context.t()))

  # type var
  @typep a :: any()
  @typep b :: any()

  defguard is_thunk(f) when is_function(f, 0)

  @spec new(a(), t(a)) :: t(a)
  def new(car, cdr) do
    %__MODULE__{car: car, cdr: cdr}
  end

  @spec cons(a(), t(a)) :: t(a)
  def cons(car, cdr) do
    fn -> new(car, cdr) end
  end

  @spec empty :: t()
  def empty do
    nil
  end

  @spec singleton(a) :: t(a)
  def singleton(a) do
    new(a, nil)
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
      nil -> concat(xs)
      f when is_thunk(f) -> fn -> concat(new(f.(), xs)) end
      %{car: y, cdr: ys} -> new(y, fn -> concat(new(ys, xs)) end)
    end
  end

  @spec mplus(t(a), t(a)) :: t(a)
  def mplus(nil, s2), do: s2
  def mplus(s1, nil), do: s1
  def mplus(f, s2) when is_thunk(f), do: fn -> mplus(s2, f.()) end
  def mplus(%{car: x, cdr: xs}, s2), do: new(x, fn -> mplus(s2, xs) end)

  @doc """
  This mplus function is the same implementation used in condi in
  other miniKanren implementations. It's generally more effective in
  searching the state tree comparing to those that uses
  interleave/concat. However, as you can see in the example below, the
  search order is difficult keep track of.

  You can use `condem` function to use this implementation.

  iex> [
  ...>   [0, 1, 3, 4],
  ...>   [2, 5, 7],
  ...>   [6, 8],
  ...>   [9]
  ...> ]
  ...> |> Enum.map(&from_list/1)
  ...> |> from_list()
  ...> |> mplus_many()
  ...> |> to_list()
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  """
  @spec mplus_many(t(t(a))) :: t(a)
  def mplus_many(nil), do: nil
  def mplus_many(f) when is_thunk(f), do: fn -> mplus_many(f.()) end
  def mplus_many(%{car: s, cdr: ss}), do: mplus(s, fn -> mplus_many(ss) end)

  # the input stream needs to be of element type Subst.t()
  @spec bind_goal(t(Subst.t()), goal()) :: t(Subst.t())
  def bind_goal(nil, _g), do: nil
  def bind_goal(f, g) when is_thunk(f), do: fn -> bind_goal(f.(), g) end

  def bind_goal(%{car: x, cdr: xs}, g) do
    mplus(bind_goal(xs, g), g.(x))
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
    cons(f.(x), fn -> map(xs, f) end)
  end

  @spec take(t(a), non_neg_integer()) :: t(a)
  def take(_s, 0), do: nil
  def take(nil, _n), do: nil
  def take(f, n) when is_thunk(f), do: fn -> take(f.(), n) end

  def take(%{car: x, cdr: xs}, n) do
    cons(x, fn -> take(xs, n - 1) end)
  end

  @spec split(t(a)) :: {a, t(a)} | nil
  def split(nil), do: nil
  def split(f) when is_thunk(f), do: split(f.())
  def split(%{car: x, cdr: xs}), do: {x, xs}

  def to_list(nil), do: []
  def to_list(f) when is_thunk(f), do: to_list(f.())
  def to_list(%{car: x, cdr: xs}), do: [x | to_list(xs)]

  def from_list([]), do: nil
  def from_list([x | xs]), do: cons(x, fn -> from_list(xs) end)

  @spec filter(t(a), (a -> boolean())) :: t(a)
  def filter(nil, _pred), do: nil
  def filter(f, pred) when is_thunk(f), do: fn -> filter(f.(), pred) end

  def filter(%{car: x, cdr: xs}, pred) do
    if pred.(x),
      do: cons(x, fn -> filter(xs, pred) end),
      else: filter(xs, pred)
  end

  @spec empty?(t()) :: boolean()
  def empty?(nil), do: true
  def empty?(f) when is_thunk(f), do: empty?(f.())
  def empty?(%{car: _, cdr: _}), do: false

  @spec to_elixir_stream(t()) :: Elixir.Stream.t()
  def to_elixir_stream(v) do
    Elixir.Stream.unfold(v, &unfold/1)
  end

  defp unfold(v) do
    case v do
      nil -> nil
      f when is_thunk(f) -> unfold(f.())
      %{car: x, cdr: xs} -> {x, xs}
    end
  end
end
