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
        # a goal that contains a fresh variable
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

  test "conde and condi" do
    x = Var.new()
    assert run(10, x, condi([[succ, eq(x, :foo)], [eq(x, :bar)]])) == []
  end

  test "trivial" do
  end

  def anyo(g) do
    condi([g, fn -> anyo(g) end])
  end

  def caro(v, l) do
    vs = Var.new()
    eq([v | vs], l)
  end

  def cdro(vs, l) do
    v = Var.new()
    eq([v | vs], l)
  end

  def conso(v, vs, l) do
    eq([v | vs], l)
  end

  def pairo(p) do
    a = Var.new()
    b = Var.new()
    eq([a | b], p)
  end

  def nullo(l) do
    eq(l, [])
  end

  def listo(l) do
    conde([
      nullo(l),
      fn ->
        x = Var.new()
        xs = Var.new()

        [
          conso(x, xs, l),
          listo(xs)
        ]
      end
    ])
  end
end
