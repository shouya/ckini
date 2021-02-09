defmodule CkiniTest do
  use ExUnit.Case
  doctest Ckini.Stream
  doctest Ckini

  import Ckini
  alias Ckini.{Var, Stream}

  defmodule Demo do
    import Ckini
    alias Ckini.Var

    def readme_demo do
      x = Var.new()
      y = Var.new()
      z = Var.new()

      run({x, y, z}, [
        # simple goal
        eq(y, 1),
        # a conde goal
        conde([eq(y, 2), eq(z, 3), eq(x, 4)]),
        # you can create logic variable any time
        fn ->
          t = Var.new()
          eq(x, [y, z, t, "hello"])
        end
      ])
    end
  end

  test "demo in README should work correctly" do
    assert [{[1, 3, :_0, "hello"], 1, 3}] == Demo.readme_demo()
  end

  test "listo" do
    v = Var.new()

    assert run(6, v, listo(v)) == [
             [],
             [:_0],
             [:_0, :_1],
             [:_0, :_1, :_2],
             [:_0, :_1, :_2, :_3],
             [:_0, :_1, :_2, :_3, :_4]
           ]
  end

  def listo(l) do
    x = Var.new()
    xs = Var.new()

    conde([
      eq(l, []),
      fn -> [eq([x | xs], l), listo(xs)] end
    ])
  end

  @tag timeout: 1000
  test "appendo" do
    x = Var.new(:x)
    y = Var.new(:y)

    assert run({x, y}, appendo(x, y, [1, 2, 3, 4])) == [
             {[], [1, 2, 3, 4]},
             {[1], [2, 3, 4]},
             {[1, 2], [3, 4]},
             {[1, 2, 3], [4]},
             {[1, 2, 3, 4], []}
           ]

    assert run({x, y}, appendo2(x, y, [1, 2, 3, 4])) ==
             run({x, y}, appendo(x, y, [1, 2, 3, 4]))
  end

  def appendo(l, s, out) do
    conde([
      [eq(l, []), eq(s, out)],
      fn ->
        a = Var.new(:a)
        d = Var.new(:d)

        [
          eq([a | d], l),
          fn ->
            res = Var.new(:res)
            [eq([a | res], out), appendo(d, s, res)]
          end
        ]
      end
    ])
  end

  def appendo2(l, s, out) do
    a = Var.new(:a)
    d = Var.new(:d)
    res = Var.new(:res)

    conde([
      [eq(l, []), eq(s, out)],
      [eq([a | res], out), eq([a | d], l), appendo(d, s, res)]
    ])
  end

  test "condi and conde" do
    teacupo = fn v ->
      conde([eq(v, :tea), eq(v, :cup)])
    end

    x = Var.new()

    # condi will interleave the goals
    assert run(x, condi([teacupo.(x), eq(x, 0)])) == [:tea, 0, :cup]

    # conde will perform depth-first search
    assert run(x, conde([teacupo.(x), eq(x, 0)])) == [:tea, :cup, 0]
  end

  test "project" do
    q = Var.new()

    # example from TRS
    assert run(q, [
             eq(q, false),
             project(q, fn q ->
               eq(not (not q), q)
             end)
           ])
  end
end
