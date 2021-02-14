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
      fn -> valid_colso(b) end,
      fn ->
        bs = Var.new()
        [staircaseo(b, bs, &shift_lefto/2), valid_diagonalo(bs)]
      end,
      fn ->
        bs = Var.new()
        [staircaseo(b, bs, &shift_righto/2), valid_diagonalo(bs)]
      end
    ])
  end

  def valid_colso(b) do
    b_cols = Var.new()
    [transposeo(b, b_cols), eacho(&single_queeno/1, b_cols)]
  end

  def valid_diagonalo(b) do
    b_cols = Var.new()
    [transposeo(b, b_cols), eacho(&at_most_one_queeno/1, b_cols)]
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

  def staircaseo(b1, b2, shifto) do
    conde([
      fn ->
        e = Var.new()
        [eq(b1, [e]), eq(b2, [e])]
      end,
      fn ->
        [b1hd, b1tl, b1tls, b2tl] = Var.new_many(4)

        [
          eq([b1hd | b1tl], b1),
          eq([b1hd | b2tl], b2),
          mapo(shifto, b1tl, b1tls),
          staircaseo(b1tls, b2tl, shifto)
        ]
      end
    ])
  end

  def shift_lefto(i, o) do
    t = Var.new()
    [eq([Var.new() | t], i), appendo(t, [0], o)]
  end

  def shift_righto(i, o) do
    init = Var.new()
    [eq([0 | init], o), inito(i, init)]
  end

  def inito(i, o) do
    conde([
      fn -> [eq([Var.new()], i), eq([], o)] end,
      fn ->
        [ihd, itl, otl] = Var.new_many(3)
        [eq([ihd | itl], i), eq([ihd | otl], o), inito(itl, otl)]
      end
    ])
  end

  def appendo(x, y, o) do
    conde([
      fn -> [eq(x, []), eq(y, o)] end,
      fn ->
        [xh, xt, ot] = Var.new_many(3)

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

  test "staircaseo works correctly" do
    p = Var.new()

    input = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

    assert [[[1, 2, 3], [5, 6, 0], [9, 0, 0]]] =
             run(p, staircaseo(input, p, &shift_lefto/2))

    # assert [[[1, 2, 3], [0, 4, 5], [0, 0, 7]]] =
    #          run(p, staircaseo(input, p, &shift_righto/2))

    # assert [[[1, 2, 3], [0, 4, 5], [0, 0, 7]]] =
    #          run(1, p, mapo(&shift_lefto/2, p, input))
  end

  def ido(x, y), do: eq(x, y)

  @tag timeout: 600_000
  test "valid_boardo works correctly" do
    p = Var.new()

    for board <- run(10, p, valid_boardo(p)) do
      print_board(board)
      IO.puts("---")
    end
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
