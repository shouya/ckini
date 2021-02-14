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

  def valid_lineo(l) do
    xs = Var.new()

    condi([
      fn -> [eq([1 | xs], l), all_zero_lineo(xs)] end,
      fn -> [eq([0 | xs], l), valid_lineo(xs)] end
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
      fn -> boardo(b) end,
      fn -> eacho(&valid_lineo/1, b) end,
      fn ->
        b_cols = Var.new()
        [transposeo(b, b_cols), eacho(&valid_lineo/1, b_cols)]
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
        [eq(l1, [x | xs]), eq(l2, [y | ys]), fo.(x, y), mapo(fo, xs, ys)]
      end
    ])
  end

  def hdo(l, x), do: eq(l, [x | Var.new()])
  def tlo(l, xs), do: eq(l, [Var.new() | xs])

  def staircase_rotateo(b1, b2) do
    conde([
      fn ->
        [l1, l2] = Var.new_many(2)

        [
          eq(b1, [l1]),
          eq(b2, [l2]),
          rotateo(l1, l2)
        ]
      end,
      fn ->
        [b1r, b1rtl, b2r, b2rtl] = Var.new_many(4)

        [
          mapo(&rotateo/2, b1, b1r),
          mapo(&rotateo/2, , )
        ]
      end
    ])
  end

  def rotateo(i, o) do
    [h, t] = Var.new_many(2)
    [eq([h | t], i), appendo(t, [h], o)]
  end

  def appendo(x, y, o) do
    conde([
      fn -> [eq(x, []), eq(y, o)] end,
      fn ->
        [xh, xt, ot] = Var.new_many(2)

        [
          eq(x, [xh | xt]),
          eq([xh | ot], o),
          appendo(xt, y, ot)
        ]
      end
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

  test "boardo works correctly" do
    p = Var.new()
    assert [] == run(20, p, boardo(p))
  end

  test "valid_lineo works correctly" do
    p = Var.new()

    for line <- run(20, p, valid_lineo(p)) do
      assert 1 == Enum.reduce(line, &(&1 + &2))
    end
  end

  test "transposeo works correctly" do
    p = Var.new()

    assert [[[1, 4], [2, 5 | :_0], [3, 6 | :_1]]] =
             run(p, transposeo(p, [[1, 2, 3], [4, 5, 6]]))
  end

  @tag timeout: 10_000
  test "valid_boardo works correctly" do
    p = Var.new()

    for board <- run(10, p, valid_boardo(p)) do
      print_board(board)
      IO.puts("")
    end
  end

  test "find all solutions" do
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
  end
end
