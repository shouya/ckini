defmodule QueenTest do
  @moduledoc "a formulation of the eight queen problem"

  use ExUnit.Case
  use Ckini

  @n 4

  def lengtho(l, 0), do: eq(l, [])

  def lengtho(l, n) when n > 0 do
    fresh [x, xs] do
      eq([x | xs], l)
      lengtho(xs, n - 1)
    end
  end

  def lengtho(_, _), do: fail()

  def cello(c) do
    condi do
      _ -> eq(c, 0)
      _ -> eq(c, 1)
    end
  end

  def eacho(predo, l) do
    condi do
      _ ->
        eq([], l)

      [x, xs] ->
        eq([x | xs], l)
        predo.(x)
        eacho(predo, xs)
    end
  end

  def lineo(l) do
    all do
      lengtho(l, @n)
      eacho(&cello/1, l)
    end
  end

  def boardo(b) do
    all do
      lengtho(b, @n)
      eacho(&lineo/1, b)
    end
  end

  def single_queeno(l) do
    condi do
      xs ->
        eq([1 | xs], l)
        all_zero_lineo(xs)

      xs ->
        eq([0 | xs], l)
        single_queeno(xs)
    end
  end

  def at_most_one_queeno(l) do
    condi do
      _ ->
        eq([], l)

      xs ->
        eq([1 | xs], l)
        all_zero_lineo(xs)

      xs ->
        eq([0 | xs], l)
        at_most_one_queeno(xs)
    end
  end

  defp all_zero_lineo(l) do
    condi do
      _ ->
        eq(l, [])

      [x, xs] ->
        eq([x | xs], l)
        eq(x, 0)
        all_zero_lineo(xs)
    end
  end

  def valid_boardo(b) do
    fresh {bb, ls1, ls2} do
      boardo(b)
      eacho(&single_queeno/1, b)
      valid_colso(b)

      all_diagonalo(b, ls1)
      eacho(&at_most_one_queeno/1, ls1)

      reverseo(b, bb)
      all_diagonalo(bb, ls2)
      eacho(&at_most_one_queeno/1, ls2)
    end
  end

  def valid_colso(b) do
    fresh b_cols do
      transposeo(b, b_cols)
      eacho(&single_queeno/1, b_cols)
    end
  end

  def all_diagonalo(b, ls) do
    fresh {b_trans, ls1, ls2, tmp} do
      bottom_left_diagonalo(b, ls1)
      appendo(ls1, ls2, ls)
      transposeo(b, [tmp | b_trans])
      bottom_left_diagonalo(b_trans, ls2)
    end
  end

  def bottom_left_diagonalo(b, ls) do
    condi do
      tmp ->
        eq([tmp], b)
        eq([], ls)

      [bh, bt, lsh, lst] ->
        eq([bh | bt], b)
        eq([lsh | lst], ls)
        diagonalo(b, lsh)
        bottom_left_diagonalo(bt, lst)
    end
  end

  def diagonalo(b, l) do
    condi do
      _ ->
        eq([], b)
        eq([], l)

      [r1, x, btl, btltl, ltl, tmp] ->
        eq([r1 | btl], b)
        eq([x | tmp], r1)
        eq([x | ltl], l)
        mapo(&tlo/2, btl, btltl)
        diagonalo(btltl, ltl)
    end
  end

  def reverseo(l1, l2) do
    condi do
      _ ->
        eq(l1, [])
        eq(l2, [])

      [x, xs, r] ->
        eq(l1, [x | xs])
        reverseo(xs, r)
        appendo(r, [x], l2)
    end
  end

  def transposeo(l1, l2) do
    condi do
      tmp ->
        eq(l1, [[] | tmp])
        eq(l2, [])

      [l1tl, l2hd, l2tl] ->
        eq([l2hd | l2tl], l2)
        mapo(&hdo/2, l1, l2hd)
        mapo(&tlo/2, l1, l1tl)
        transposeo(l1tl, l2tl)
    end
  end

  def mapo(fo, l1, l2) do
    condi do
      _ ->
        eq(l1, [])
        eq(l2, [])

      [x, xs, y, ys] ->
        eq(l1, [x | xs])
        eq(l2, [y | ys])
        fo.(x, y)
        mapo(fo, xs, ys)
    end
  end

  def hdo(l, x) do
    fresh(tmp, do: eq(l, [x | tmp]))
  end

  def tlo(l, xs) do
    fresh(tmp, do: eq(l, [tmp | xs]))
  end

  def appendo(x, y, o) do
    condi do
      _ ->
        eq(x, [])
        eq(y, o)

      [xh, xt, ot] ->
        eq(x, [xh | xt])
        eq([xh | ot], o)
        appendo(xt, y, ot)
    end
  end

  test "lengtho works correctly" do
    assert [[:_0, :_1, :_2]] == run(p, do: lengtho(p, 3))
  end

  test "lineo works correctly" do
    for line <- run(20, p, do: lineo(p)) do
      assert length(line) == @n
    end
  end

  test "single_queeno works correctly" do
    for line <- run(20, p, do: single_queeno(p)) do
      assert 1 == Enum.reduce(line, &(&1 + &2))
    end
  end

  test "transposeo works correctly" do
    assert [[[1, 4], [2, 5 | :_0], [3, 6 | :_1]]] =
             run(p, do: transposeo(p, [[1, 2, 3], [4, 5, 6]]))
  end

  test "reverseo works" do
    assert [[4, 3, 2, 1]] == run(p, do: reverseo([1, 2, 3, 4], p))
  end

  # it runs for about 30 seconds to get the result for n=4 :(
  @tag :skip
  @tag timeout: 600_000
  test "valid_boardo works correctly" do
    assert [_, _] = run(10, p, do: valid_boardo(p))
  end

  def print_board(board) do
    for line <- board do
      for cell <- line do
        case cell do
          0 -> ". "
          1 -> "o "
        end
      end
      |> Enum.join("")
    end
    |> Enum.join("\n")
    |> IO.puts()

    IO.puts("")
  end
end
