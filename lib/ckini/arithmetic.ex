defmodule Ckini.Arithmetic do
  @moduledoc """
  Implementation of arithmetic using a binary representation of
  natural numbers.

  All implementations are copied from chapter 6 of <Relational
  Programming in miniKanren> by William E. Byrd. The implementations
  are claimed to refutational complete.

  I personally don't understand how anything works after pluso. So
  I'll only translating the definition to Ckini.
  """

  use Ckini, style: :functional

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

  def minuso(n, m, k) do
    pluso(m, k, n)
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
  def gt1o(n) do
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

  @doc """
  iex> use Ckini
  iex> n = Var.new()
  iex> run(n, mulo(from_number(5), from_number(4), n))
  [from_number(20)]
  iex> m = Var.new()
  iex> run({m, n}, mulo(m, n, from_number(12)))
  [{from_number(1), from_number(12)},
   {from_number(12), from_number(1)},
   {from_number(2), from_number(6)},
   {from_number(4), from_number(3)},
   {from_number(6), from_number(2)},
   {from_number(3), from_number(4)}]
  """
  def mulo(n, m, p) do
    [x, y, z] = Var.new_many(3)
    rec = fn n, m, p -> fn -> mulo(n, m, p) end end
    odd_m = fn x, n, m, p -> fn -> odd_mulo(x, n, m, p) end end

    conde([
      [eq([], n), eq([], p)],
      [eq([], m), eq([], p), poso(n)],
      [eq([1], n), eq(m, p), poso(m)],
      [eq([1], m), eq(n, p), gt1o(n)],
      [
        eq([0 | x], n),
        eq([0 | z], p),
        poso(x),
        poso(z),
        gt1o(m),
        rec.(x, m, z)
      ],
      [eq([1 | x], n), eq([0 | y], m), poso(x), poso(y), rec.(m, n, p)],
      [eq([1 | x], n), eq([1 | y], m), poso(x), poso(y), odd_m.(x, n, m, p)]
    ])
  end

  defp odd_mulo(x, n, m, p) do
    q = Var.new()

    [
      fn -> bound_mulo(q, p, n, m) end,
      fn -> mulo(x, m, q) end,
      fn -> pluso([0 | q], m, p) end
    ]
  end

  defp bound_mulo(q, p, n, m) do
    [x, y, z, a] = Var.new_many(4)
    [tmp1, tmp2] = Var.new_many(2)

    conde([
      [eq([], q), eq([tmp1 | tmp2], p)],
      [
        eq([tmp1 | x], q),
        eq([tmp2 | y], p),
        fn ->
          conde([
            [eq([], n), eq([a | z], m), fn -> bound_mulo(x, y, z, []) end],
            [eq([a | z], n), fn -> bound_mulo(x, y, z, m) end]
          ])
        end
      ]
    ])
  end

  @doc """
  Relation for n < m.

  iex> use Ckini
  iex> n = Var.new()
  iex> run(n, lto(n, from_number(3)))
  [from_number(0), from_number(1), from_number(2)]
  """
  def lto(n, m) do
    x = Var.new()

    conde([
      fn -> length_lto(n, m) end,
      [fn -> length_eqo(n, m) end, poso(x), fn -> pluso(n, x, m) end]
    ])
  end

  @doc """
  Relation for n < m.

  iex> use Ckini
  iex> n = Var.new()
  iex> run(n, leo(n, from_number(3)))
  [from_number(0), from_number(1), from_number(2), from_number(3)]
  """
  def leo(n, m) do
    conde([fn -> lto(n, m) end, eq(n, m)])
  end

  @doc """
  Relation for n = m * q + r, with 0 <= r < m.

  In this example we test for some n,m pairs that satisfies n = m * 3 + 1.

  iex> use Ckini
  iex> n = Var.new()
  iex> for i <- 2..10 do
  ...>   r = run(n, divo(n, from_number(i), from_number(3), from_number(1)))
  ...>   assert [from_number(i*3+1)] == r
  ...> end
  """
  def divo(n, m, q, r) do
    conde([
      [eq(q, []), eq(r, n), fn -> lto(n, m) end],
      [
        eq(q, [1]),
        length_eqo(n, m),
        fn -> pluso(r, m, n) end,
        fn -> lto(r, m) end
      ],
      fn ->
        [nh, nl, qh, ql, qlm, qlmr, rr, rh] = Var.new_many(8)

        [
          fn -> length_lto(m, n) end,
          fn -> lto(r, m) end,
          poso(q),
          fn -> splito(n, r, nl, nh) end,
          fn -> splito(q, r, ql, qh) end,
          conde([
            [
              eq([], nh),
              eq([], qh),
              fn -> minuso(nl, r, qlm) end,
              fn -> mulo(ql, m, qlm) end
            ],
            [
              poso(nh),
              fn -> mulo(ql, m, qlm) end,
              fn -> pluso(qlm, r, qlmr) end,
              fn -> minuso(qlmr, nl, rr) end,
              fn -> splito(rr, r, [], rh) end,
              fn -> divo(nh, m, qh, rh) end
            ]
          ])
        ]
      end
    ])
  end

  # holds if n = 2^(s+1) * l + h where s = len(r) and h < 2^(s+1)
  defp splito(n, r, l, h) do
    [a, b, nn, rr, ll] = Var.new_many(5)

    rec = fn n, r, l, h -> fn -> splito(n, r, l, h) end end

    conde([
      [eq([], n), eq([], l), eq([], h)],
      [eq([0, b | nn], n), eq([], r), eq([], l), eq([b | nn], h)],
      [eq([1 | nn], n), eq([], r), eq([1], l), eq(nn, h)],
      [
        eq([0, b | nn], n),
        eq([a | rr], r),
        eq([], l),
        rec.([b | nn], rr, [], h)
      ],
      [
        eq([1 | nn], n),
        eq([a | rr], r),
        eq([1], l),
        rec.(nn, rr, [], h)
      ],
      [
        eq([b | nn], n),
        eq([a | rr], r),
        eq([b | ll], l),
        poso(ll),
        rec.(nn, rr, ll, h)
      ]
    ])
  end

  defp length_eqo(n, m) do
    [a, b, x, y] = Var.new_many(4)
    recur = fn n, m -> fn -> length_eqo(n, m) end end

    conde([
      [eq(n, []), eq(m, [])],
      [eq(n, [1]), eq(m, [1])],
      [eq([a | x], n), eq([b | y], m), poso(x), poso(y), recur.(x, y)]
    ])
  end

  defp length_lto(n, m) do
    [a, b, x, y] = Var.new_many(4)
    recur = fn n, m -> fn -> length_lto(n, m) end end

    conde([
      [eq(n, []), poso(m)],
      [eq(n, [1]), gt1o(m)],
      [eq([a | x], n), eq([b | y], m), poso(x), poso(y), recur.(x, y)]
    ])
  end

  defp length_leo(n, m) do
    conde([fn -> length_lto(n, m) end, fn -> length_eqo(n, m) end])
  end

  @doc """
  This function doesn't spit out complete result yet. More investigation needed.

  See https://github.com/shouya/ckini/issues/1
  """
  def logo(n, b, q, r) do
    [a, bb, dd, add, ddd, bw1, bw, nw, nw1, ql1, ql] = Var.new_many(11)
    [qq, bwq1, bql, qh, s, qdh, qd, bqd, bq1, bq] = Var.new_many(10)

    conde([
      fn -> [eq([1], n), eq([], q), eq([], r), poso(b)] end,
      fn -> [eq([], q), lto(n, b), pluso(r, [1], n)] end,
      fn -> [eq([1], q), gt1o(b), length_eqo(n, b), pluso(r, b, n)] end,
      fn -> [eq([1], b), poso(q), pluso(r, [1], n)] end,
      fn -> [eq([], b), poso(q), eq(r, n)] end,
      fn ->
        [
          eq([a, bb | dd], n),
          eq([0, 1], b),
          poso(dd),
          fn -> exp2o(n, [], q) end,
          fn -> splito(n, dd, r, s) end
        ]
      end,
      fn ->
        [
          fn -> conde([eq([1, 1], b), eq([a, bb, add | ddd], b)]) end,
          fn -> length_lto(b, n) end,
          fn -> exp2o(b, [], bw1) end,
          fn -> pluso(bw1, [1], bw) end,
          fn -> length_lto(q, n) end,
          fn -> pluso(q, [1], qq) end,
          fn -> mulo(bw, qq, bwq1) end,
          fn -> lto(nw1, bwq1) end,
          fn -> exp2o(n, [], nw1) end,
          fn -> pluso(nw1, [1], nw) end,
          fn -> divo(nw, bw, ql1, s) end,
          fn -> pluso(ql, [1], ql1) end,
          fn -> length_leo(ql, q) end,
          fn -> repeated_mulo(b, ql, bql) end,
          fn -> divo(nw, bw1, qh, s) end,
          fn -> pluso(ql, qdh, qh) end,
          fn -> pluso(ql, qd, q) end,
          fn -> leo(qd, qdh) end,
          fn -> repeated_mulo(b, qd, bqd) end,
          fn -> mulo(bql, bqd, bq) end,
          fn -> mulo(b, bq, bq1) end,
          fn -> pluso(bq, r, n) end,
          fn -> lto(n, bq1) end
        ]
      end
    ])
  end

  defp exp2o(n, b, q) do
    [s, bb, nh, qq] = Var.new_many(4)

    conde([
      fn -> [eq([1], n), eq([], q)] end,
      fn -> [eq([1], q), gt1o(n), splito(n, b, s, [1])] end,
      fn ->
        [
          eq([0 | qq], q),
          poso(qq),
          length_lto(b, n),
          appendo(b, [1 | b], bb),
          fn -> exp2o(n, bb, qq) end
        ]
      end,
      fn ->
        [
          eq([1 | qq], q),
          poso(qq),
          poso(nh),
          splito(n, b, s, nh),
          appendo(b, [1 | b], bb),
          fn -> exp2o(nh, bb, qq) end
        ]
      end
    ])
  end

  defp repeated_mulo(n, q, nq) do
    conde([
      fn -> [eq([], q), eq([1], nq), poso(n)] end,
      fn -> [eq([1], q), eq(n, nq)] end,
      fn ->
        [qq, nq1] = Var.new_many(2)

        [
          gt1o(q),
          pluso(qq, [1], q),
          fn -> repeated_mulo(n, qq, nq1) end,
          mulo(nq1, n, nq)
        ]
      end
    ])
  end

  def appendo(l, s, out) do
    [a, d, res] = Var.new_many(3)

    conde([
      [eq(l, []), eq(s, out)],
      [eq([a | res], out), eq([a | d], l), fn -> appendo(d, s, res) end]
    ])
  end
end
