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

  use Ckini

  require Integer

  @doc """
  Convert an Elixir integer (>= 0) to a list-encoded
  numeral representing the number to be used by the relations.
  """
  def from_number(0), do: []
  def from_number(1), do: [1]
  def from_number(n) when Integer.is_even(n), do: [0 | from_number(div(n, 2))]
  def from_number(n), do: [1 | from_number(div(n, 2))]

  @doc """
  Convert a list-encoded numeral back to an Elixir integer.
  """
  def to_number([]), do: 0
  def to_number([1]), do: 1
  def to_number([0 | x]), do: 2 * to_number(x)
  def to_number([1 | x]), do: 2 * to_number(x) + 1

  @doc """
  Relation for n + m = k.

  ## Examples

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

  @doc """
  Relation for n - m = k.

  See `pluso/3`.
  """
  def minuso(n, m, k) do
    pluso(m, k, n)
  end

  defp addero(d, n, m, r) do
    conde do
      _ ->
        [eq(0, d), eq([], m), eq(n, r)]

      _ ->
        [eq(0, d), eq([], n), eq(m, r), poso(m)]

      _ ->
        [eq(1, d), eq([], m), addero(0, n, [1], r)]

      _ ->
        [eq(1, d), eq([], n), poso(m), addero(0, [1], m, r)]

      {a, c} ->
        [eq([1], n), eq([1], m), eq([a, c], r), full_addero(d, 1, 1, a, c)]

      _ ->
        [eq([1], n), gen_addero(d, n, m, r)]

      _ ->
        [eq([1], m), gt1o(n), gt1o(r), addero(d, [1], n, r)]

      _ ->
        [gt1o(n), gen_addero(d, n, m, r)]
    end
  end

  # gen_addero calculates d+n+m=r, provided m>1, r>1, and n>0.
  defp gen_addero(d, n, m, r) do
    fresh {a, b, c, e, x, y, z} do
      eq([a | x], n)
      eq([b | y], m)
      eq([c | z], r)
      poso(y)
      poso(z)
      full_addero(d, a, b, c, e)
      addero(e, x, y, z)
    end
  end

  defp poso(n) do
    fresh {a, d} do
      eq([a | d], n)
    end
  end

  @doc """
  Relation that holds for n > 1.
  """
  def gt1o(n) do
    fresh {a, b, d} do
      eq([a, b | d], n)
    end
  end

  defp full_addero(b, x, y, r, c) do
    conde do
      _ -> [eq(b, 0), eq(x, 0), eq(y, 0), eq(r, 0), eq(c, 0)]
      _ -> [eq(b, 0), eq(x, 0), eq(y, 1), eq(r, 1), eq(c, 0)]
      _ -> [eq(b, 0), eq(x, 1), eq(y, 0), eq(r, 1), eq(c, 0)]
      _ -> [eq(b, 1), eq(x, 0), eq(y, 0), eq(r, 1), eq(c, 0)]
      _ -> [eq(b, 0), eq(x, 1), eq(y, 1), eq(r, 0), eq(c, 1)]
      _ -> [eq(b, 1), eq(x, 0), eq(y, 1), eq(r, 0), eq(c, 1)]
      _ -> [eq(b, 1), eq(x, 1), eq(y, 0), eq(r, 0), eq(c, 1)]
      _ -> [eq(b, 1), eq(x, 1), eq(y, 1), eq(r, 1), eq(c, 1)]
    end
  end

  @doc """
  Relation for n * m = p.

  ## Examples

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
    conde do
      _ ->
        [eq([], n), eq([], p)]

      _ ->
        [eq([], m), eq([], p), poso(n)]

      _ ->
        [eq([1], n), eq(m, p), poso(m)]

      _ ->
        [eq([1], m), eq(n, p), gt1o(n)]

      {z, x} ->
        eq([0 | x], n)
        eq([0 | z], p)
        poso(x)
        poso(z)
        gt1o(m)
        mulo(x, m, z)

      {x, y} ->
        [eq([1 | x], n), eq([0 | y], m), poso(x), poso(y), mulo(m, n, p)]

      {x, y} ->
        [eq([1 | x], n), eq([1 | y], m), poso(x), poso(y), odd_mulo(x, n, m, p)]
    end
  end

  defp odd_mulo(x, n, m, p) do
    fresh q do
      bound_mulo(q, p, n, m)
      mulo(x, m, q)
      pluso([0 | q], m, p)
    end
  end

  defp bound_mulo(q, p, n, m) do
    conde do
      {tmp1, tmp2} ->
        eq([], q)
        eq([tmp1 | tmp2], p)

      {x, y, z, tmp1, tmp2} ->
        eq([tmp1 | x], q)
        eq([tmp2 | y], p)

        condi do
          a ->
            eq([], n)
            eq([a | z], m)
            bound_mulo(x, y, z, [])

          a ->
            eq([a | z], n)
            bound_mulo(x, y, z, m)
        end
    end
  end

  @doc """
  Relation for n < m.

  ## Examples

      iex> use Ckini
      iex> n = Var.new()
      iex> run(n, lto(n, from_number(3)))
      [from_number(0), from_number(1), from_number(2)]
  """
  def lto(n, m) do
    conde do
      _ ->
        length_lto(n, m)

      x ->
        length_eqo(n, m)
        poso(x)
        pluso(n, x, m)
    end
  end

  @doc """
  Relation for n < m.

  ## Examples

      iex> use Ckini
      iex> n = Var.new()
      iex> run(n, leo(n, from_number(3)))
      [from_number(0), from_number(1), from_number(2), from_number(3)]
  """
  def leo(n, m) do
    conde do
      _ -> lto(n, m)
      _ -> eq(n, m)
    end
  end

  @doc """
  Relation for n = m * q + r, with 0 <= r < m.

  ## Examples

  In this example we test for some n,m pairs that satisfies n = m * 3 + 1.

      iex> use Ckini
      iex> n = Var.new()
      iex> for i <- 2..10 do
      ...>   r = run(n, divo(n, from_number(i), from_number(3), from_number(1)))
      ...>   assert [from_number(i*3+1)] == r
      ...> end
  """
  def divo(n, m, q, r) do
    conde do
      _ ->
        eq(q, [])
        eq(r, n)
        lto(n, m)

      _ ->
        eq(q, [1])
        length_eqo(n, m)
        pluso(r, m, n)
        lto(r, m)

      {nh, nl, qh, ql, qlm, qlmr, rr, rh} ->
        length_lto(m, n)
        lto(r, m)
        poso(q)
        splito(n, r, nl, nh)
        splito(q, r, ql, qh)

        conde do
          _ ->
            eq([], nh)
            eq([], qh)
            minuso(nl, r, qlm)
            mulo(ql, m, qlm)

          _ ->
            poso(nh)
            mulo(ql, m, qlm)
            pluso(qlm, r, qlmr)
            minuso(qlmr, nl, rr)
            splito(rr, r, [], rh)
            divo(nh, m, qh, rh)
        end
    end
  end

  # holds if n = 2^(s+1) * l + h where s = len(r) and h < 2^(s+1)
  defp splito(n, r, l, h) do
    conde do
      _ ->
        [eq([], n), eq([], l), eq([], h)]

      {b, nn} ->
        [eq([0, b | nn], n), eq([], r), eq([], l), eq([b | nn], h)]

      nn ->
        [eq([1 | nn], n), eq([], r), eq([1], l), eq(nn, h)]

      {a, b, nn, rr} ->
        eq([0, b | nn], n)
        eq([a | rr], r)
        eq([], l)
        splito([b | nn], rr, [], h)

      {a, nn, rr} ->
        eq([1 | nn], n)
        eq([a | rr], r)
        eq([1], l)
        splito(nn, rr, [], h)

      {a, b, nn, rr, ll} ->
        eq([b | nn], n)
        eq([a | rr], r)
        eq([b | ll], l)
        poso(ll)
        splito(nn, rr, ll, h)
    end
  end

  defp length_eqo(n, m) do
    conde do
      _ ->
        eq(n, [])
        eq(m, [])

      _ ->
        eq(n, [1])
        eq(m, [1])

      {a, b, x, y} ->
        eq([a | x], n)
        eq([b | y], m)
        poso(x)
        poso(y)
        length_eqo(x, y)
    end
  end

  defp length_lto(n, m) do
    conde do
      _ ->
        eq(n, [])
        poso(m)

      _ ->
        eq(n, [1])
        gt1o(m)

      {a, b, x, y} ->
        eq([a | x], n)
        eq([b | y], m)
        poso(x)
        poso(y)
        length_lto(x, y)
    end
  end

  defp length_leo(n, m) do
    conde do
      _ -> length_lto(n, m)
      _ -> length_eqo(n, m)
    end
  end

  @doc """
  Relation that holds for n = b^q + r.

  This relation doesn't spit out complete result yet. More investigation needed.

  See https://github.com/shouya/ckini/issues/1.
  """
  def logo(n, b, q, r) do
    conde do
      _ ->
        eq([1], n)
        eq([], q)
        eq([], r)
        poso(b)

      _ ->
        eq([], q)
        lto(n, b)
        pluso(r, [1], n)

      _ ->
        eq([1], q)
        gt1o(b)
        length_eqo(n, b)
        pluso(r, b, n)

      _ ->
        eq([1], b)
        poso(q)
        pluso(r, [1], n)

      _ ->
        eq([], b)
        poso(q)
        eq(r, n)

      {a, bb, dd, s} ->
        eq([a, bb | dd], n)
        eq([0, 1], b)
        poso(dd)
        exp2o(n, [], q)
        splito(n, dd, r, s)

      {a, bb, add, ddd, bw, bw1, qq, bwq1, nw1, nw, ql, ql1, s, bql, qh, qdh,
       qd, bqd, bq, bq1} ->
        conde do
          _ -> eq([1, 1], b)
          _ -> eq([a, bb, add | ddd], b)
        end

        length_lto(b, n)
        exp2o(b, [], bw1)
        pluso(bw1, [1], bw)
        length_lto(q, n)
        pluso(q, [1], qq)
        mulo(bw, qq, bwq1)
        lto(nw1, bwq1)
        exp2o(n, [], nw1)
        pluso(nw1, [1], nw)
        divo(nw, bw, ql1, s)
        pluso(ql, [1], ql1)
        length_leo(ql, q)
        repeated_mulo(b, ql, bql)
        divo(nw, bw1, qh, s)
        pluso(ql, qdh, qh)
        pluso(ql, qd, q)
        leo(qd, qdh)
        repeated_mulo(b, qd, bqd)
        mulo(bql, bqd, bq)
        mulo(b, bq, bq1)
        pluso(bq, r, n)
        lto(n, bq1)
    end
  end

  defp exp2o(n, b, q) do
    conde do
      _ ->
        eq([1], n)
        eq([], q)

      s ->
        eq([1], q)
        gt1o(n)
        splito(n, b, s, [1])

      {qq, bb} ->
        eq([0 | qq], q)
        poso(qq)
        length_lto(b, n)
        appendo(b, [1 | b], bb)
        exp2o(n, bb, qq)

      {qq, nh, bb, s} ->
        eq([1 | qq], q)
        poso(qq)
        poso(nh)
        splito(n, b, s, nh)
        appendo(b, [1 | b], bb)
        exp2o(nh, bb, qq)
    end
  end

  defp repeated_mulo(n, q, nq) do
    conde do
      _ ->
        eq([], q)
        eq([1], nq)
        poso(n)

      _ ->
        eq([1], q)
        eq(n, nq)

      {qq, nq1} ->
        gt1o(q)
        pluso(qq, [1], q)
        repeated_mulo(n, qq, nq1)
        mulo(nq1, n, nq)
    end
  end

  defp appendo(l, s, out) do
    conde do
      _ ->
        eq(l, [])
        eq(s, out)

      {a, d, res} ->
        eq([a | res], out)
        eq([a | d], l)
        appendo(d, s, res)
    end
  end
end
