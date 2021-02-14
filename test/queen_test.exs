defmodule QueenTest do
  @moduledoc "a formulation of the eight queen problem"

  use ExUnit.Case
  use Ckini

  @n 4

  def lengtho(l, 0), do: eq(l, [])

  def lengtho(l, n) when n > 0 do
    [x, xs] = Var.new_many(2)

    all([
      eq([x | xs], l),
      fn -> lengtho(xs, n - 1) end
    ])
  end

  def lengtho(_, _), do: fail()

  def cello(c) do
    conde([eq(c, 0), eq(c, 1)])
  end

  def eacho(predo, l) do
    conde([
      fn -> eq([], l) end,
      fn ->
        [x, xs] = Var.new_many(2)
        [eq([x | xs], l), predo.(x), fn -> eacho(predo, xs) end]
      end
    ])
  end

  def lineo(l) do
    all([lengtho(l, @n), eacho(&cello/1, l)])
  end

  def boardo(b) do
    all([lengtho(b, @n), eacho(&lineo/1, b)])
  end

  def single_queeno(l) do
    xs = Var.new()

    condi([
      fn -> [eq([1 | xs], l), all_zero_lineo(xs)] end,
      fn -> [eq([0 | xs], l), single_queeno(xs)] end
    ])
  end

  def at_most_one_queeno(l) do
    xs = Var.new()

    condi([
      fn -> eq([], l) end,
      fn -> [eq([1 | xs], l), all_zero_lineo(xs)] end,
      fn -> [eq([0 | xs], l), at_most_one_queeno(xs)] end
    ])
  end

  defp all_zero_lineo(l) do
    conde([
      fn -> eq(l, []) end,
      fn ->
        [x, xs] = Var.new_many(2)
        [eq([x | xs], l), eq(x, 0), all_zero_lineo(xs)]
      end
    ])
  end

  def valid_boardo(b) do
    all([
      boardo(b),
      eacho(&single_queeno/1, b),
      valid_colso(b),
      fn ->
        ls = Var.new()

        [
          all_diagonalo(b, ls),
          eacho(&at_most_one_queeno/1, ls)
        ]
      end,
      fn ->
        [bb, ls] = Var.new_many(2)

        [
          reverseo(b, bb),
          all_diagonalo(bb, ls),
          eacho(&at_most_one_queeno/1, ls)
        ]
      end
    ])
  end

  def valid_colso(b) do
    b_cols = Var.new()
    [transposeo(b, b_cols), eacho(&single_queeno/1, b_cols)]
  end

  def all_diagonalo(b, ls) do
    [b_trans, ls1, ls2] = Var.new_many(3)

    all([
      bottom_left_diagonalo(b, ls1),
      appendo(ls1, ls2, ls),
      transposeo(b, [Var.new() | b_trans]),
      bottom_left_diagonalo(b_trans, ls2)
    ])
  end

  def bottom_left_diagonalo(b, ls) do
    conde([
      fn -> [eq([Var.new()], b), eq([], ls)] end,
      fn ->
        [bh, bt, lsh, lst] = Var.new_many(4)

        [
          eq([bh | bt], b),
          eq([lsh | lst], ls),
          diagonalo(b, lsh),
          bottom_left_diagonalo(bt, lst)
        ]
      end
    ])
  end

  def diagonalo(b, l) do
    conde([
      fn -> [eq([], b), eq([], l)] end,
      fn ->
        [r1, x, btl, btltl, ltl] = Var.new_many(5)

        [
          eq([r1 | btl], b),
          eq([x | Var.new()], r1),
          eq([x | ltl], l),
          mapo(&tlo/2, btl, btltl),
          diagonalo(btltl, ltl)
        ]
      end
    ])
  end

  def reverseo(l1, l2) do
    conde([
      fn -> [eq(l1, []), eq(l2, [])] end,
      fn ->
        [x, xs, r] = Var.new_many(3)

        [
          eq(l1, [x | xs]),
          reverseo(xs, r),
          appendo(r, [x], l2)
        ]
      end
    ])
  end

  def transposeo(l1, l2) do
    conde([
      fn -> [eq(l1, [[] | Var.new()]), eq(l2, [])] end,
      fn ->
        l1tl = Var.new()
        l2hd = Var.new()
        l2tl = Var.new()

        [
          eq([l2hd | l2tl], l2),
          mapo(&hdo/2, l1, l2hd),
          mapo(&tlo/2, l1, l1tl),
          transposeo(l1tl, l2tl)
        ]
      end
    ])
  end

  def mapo(fo, l1, l2) do
    conde([
      fn -> [eq(l1, []), eq(l2, [])] end,
      fn ->
        [x, xs, y, ys] = Var.new_many(4)

        [
          eq(l1, [x | xs]),
          eq(l2, [y | ys]),
          fo.(x, y),
          mapo(fo, xs, ys)
        ]
      end
    ])
  end

  def hdo(l, x), do: eq(l, [x | Var.new()])
  def tlo(l, xs), do: eq(l, [Var.new() | xs])

  def appendo(x, y, o) do
    [xh, xt, ot] = Var.new_many(3)

    conde([
      fn -> [eq(x, []), eq(y, o)] end,
      fn -> [eq(x, [xh | xt]), eq([xh | ot], o), appendo(xt, y, ot)] end
    ])
  end

  test "lengtho works correctly" do
    p = Var.new()
    assert [[:_0, :_1, :_2]] == run(p, lengtho(p, 3))
  end

  test "lineo works correctly" do
    p = Var.new()

    for line <- run(20, p, lineo(p)) do
      assert length(line) == @n
    end
  end

  test "single_queeno works correctly" do
    p = Var.new()

    for line <- run(20, p, single_queeno(p)) do
      assert 1 == Enum.reduce(line, &(&1 + &2))
    end
  end

  test "transposeo works correctly" do
    p = Var.new()

    assert [[[1, 4], [2, 5 | :_0], [3, 6 | :_1]]] =
             run(p, transposeo(p, [[1, 2, 3], [4, 5, 6]]))
  end

  test "reverseo works" do
    p = Var.new()
    assert [[4, 3, 2, 1]] == run(p, reverseo([1, 2, 3, 4], p))
  end

  @tag :skip
  @tag timeout: 600_000
  test "valid_boardo works correctly" do
    p = Var.new()

    assert [_, _] = run(10, p, valid_boardo(p))
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
