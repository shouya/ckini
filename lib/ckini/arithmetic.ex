defmodule Ckini.Arithmetic do
  @moduledoc """
  Implementation of arithmetic using a binary representation of
  natural numbers. See chapter 6 of <Relational Programming in
  miniKanren> by William E. Byrd.
  """

  use Ckini

  require Integer

  def from_number(0), do: []
  def from_number(1), do: [1]
  def from_number(n) when Integer.is_even(n), do: [0 | from_number(div(n, 2))]
  def from_number(n), do: [1 | from_number(div(n, 2))]

  def to_number([]), do: 0
  def to_number([1]), do: 1
  def to_number([0 | x]), do: 2 * to_number(x)
  def to_number([1 | x]), do: 2 * to_number(x) + 1

  @doc """
  iex> use Ckini
  iex> n = Var.new()
  iex> run(n, pluso(from_number(5), from_number(4), n))
  [from_number(9)]
  iex> m = Var.new()
  iex> run({m, n}, pluso(m, n, from_number(3)))
  [{from_number(3), from_number(0)},
   {from_number(0), from_number(3)},
   {from_number(1), from_number(2)},
   {from_number(2), from_number(1)}]
  """
  def pluso(n, m, k) do
    addero(0, n, m, k)
  end

  defp addero(d, n, m, r) do
    [a, c] = Var.new_many(2)

    conde([
      [eq(0, d), eq([], m), eq(n, r)],
      [eq(0, d), eq([], n), eq(m, r), poso(m)],
      [eq(1, d), eq([], m), fn -> addero(0, n, [1], r) end],
      [eq(1, d), eq([], n), poso(m), fn -> addero(0, [1], m, r) end],
      [eq([1], n), eq([1], m), eq([a, c], r), full_addero(d, 1, 1, a, c)],
      [eq([1], n), fn -> gen_addero(d, n, m, r) end],
      [eq([1], m), gt1o(n), gt1o(r), fn -> addero(d, [1], n, r) end],
      [gt1o(n), fn -> gen_addero(d, n, m, r) end]
    ])
  end

  # gen_addero calculates d+n+m=r, provided m>1, r>1, and n>0.
  defp gen_addero(d, n, m, r) do
    [a, b, c, e, x, y, z] = Var.new_many(7)

    [
      eq([a | x], n),
      eq([b | y], m),
      eq([c | z], r),
      poso(y),
      poso(z),
      full_addero(d, a, b, c, e),
      fn -> addero(e, x, y, z) end
    ]
  end

  defp poso(n) do
    [a, d] = Var.new_many(2)
    eq([a | d], n)
  end

  # The operator >1o
  defp gt1o(n) do
    [a, b, d] = Var.new_many(3)
    eq([a, b | d], n)
  end

  defp full_addero(b, x, y, r, c) do
    conde([
      [eq(b, 0), eq(x, 0), eq(y, 0), eq(r, 0), eq(c, 0)],
      [eq(b, 0), eq(x, 0), eq(y, 1), eq(r, 1), eq(c, 0)],
      [eq(b, 0), eq(x, 1), eq(y, 0), eq(r, 1), eq(c, 0)],
      [eq(b, 1), eq(x, 0), eq(y, 0), eq(r, 1), eq(c, 0)],
      [eq(b, 0), eq(x, 1), eq(y, 1), eq(r, 0), eq(c, 1)],
      [eq(b, 1), eq(x, 0), eq(y, 1), eq(r, 0), eq(c, 1)],
      [eq(b, 1), eq(x, 1), eq(y, 0), eq(r, 0), eq(c, 1)],
      [eq(b, 1), eq(x, 1), eq(y, 1), eq(r, 1), eq(c, 1)]
    ])
  end
end
