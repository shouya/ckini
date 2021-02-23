defmodule CkiniTest do
  use ExUnit.Case
  doctest Ckini, import: true
  doctest Ckini.Stream, import: true
  doctest Ckini.Goals, import: true
  doctest Ckini.Macro, import: true
  # doctest Ckini.Arithmetic, import: true

  use Ckini

  defmodule Demo do
    use Ckini

    def readme_demo do
      run({x, y, z}) do
        # simple goal
        eq(y, 1)
        # a conde goal
        conde do
          _ -> eq(y, 2)
          _ -> eq(z, 3)
          _ -> eq(x, 4)
        end

        # you can create logic variable any time
        fresh t do
          eq(x, [y, z, t, "hello"])
        end
      end
    end
  end

  test "demo in README should work correctly" do
    assert [{[1, 3, :_0, "hello"], 1, 3}] == Demo.readme_demo()
  end

  test "listo" do
    assert run(6, v, do: listo(v)) == [
             [],
             [:_0],
             [:_0, :_1],
             [:_0, :_1, :_2],
             [:_0, :_1, :_2, :_3],
             [:_0, :_1, :_2, :_3, :_4]
           ]
  end

  def listo(l) do
    conde do
      _ ->
        eq(l, [])

      {x, xs} ->
        eq([x | xs], l)
        listo(xs)
    end
  end

  @tag timeout: 1000
  test "appendo" do
    assert run({x, y}, do: appendo(x, y, [1, 2, 3, 4])) == [
             {[], [1, 2, 3, 4]},
             {[1], [2, 3, 4]},
             {[1, 2], [3, 4]},
             {[1, 2, 3], [4]},
             {[1, 2, 3, 4], []}
           ]
  end

  def appendo(l, s, out) do
    conde do
      _ ->
        eq(l, [])
        eq(s, out)

      {a, d, res} ->
        eq([a | d], l)
        eq([a | res], out)
        appendo(d, s, res)
    end
  end

  test "condi and conde" do
    teacupo = fn v ->
      conde do
        _ -> eq(v, :tea)
        _ -> eq(v, :cup)
      end
    end

    # condi will interleave the goals
    result =
      run(x) do
        condi do
          _ -> teacupo.(x)
          _ -> eq(x, 0)
        end
      end

    assert result == [:tea, 0, :cup]

    # conde will perform depth-first search
    result =
      run(x) do
        conde do
          _ -> teacupo.(x)
          _ -> eq(x, 0)
        end
      end

    assert result == [:tea, :cup, 0]
  end

  test "project" do
    # example from TRS
    result =
      run(q) do
        eq(q, false)

        Ckini.Functional.project(q, fn q ->
          eq(not (not q), q)
        end)
      end

    assert result == [false]
  end
end
