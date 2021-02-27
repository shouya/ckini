defmodule QuineTest do
  use ExUnit.Case
  use Ckini

  def evalo(exp, env, val) do
    condi do
      v ->
        eq([:quote, v], exp)
        absento(exp, :closure)
        eq(v, val)

      xs ->
        eq([:list | xs], exp)
        proper_listo(xs, env, val)

      _ ->
        lookupo(exp, env, val)

      {rator, rand, x, body, env_n, a} ->
        eq([rator, rand], exp)
        evalo(rator, env, [:closure, x, body, env_n])
        evalo(rand, env, a)
        evalo(body, [[x | a] | env_n], val)

      {x, body} ->
        eq([:lambda, [x], body], exp)
        eq([:closure, x, body, env], val)
    end
  end

  def proper_listo(xs, env, val) do
    condi do
      _ ->
        eq([], xs)
        eq([], val)

      {a, d, ta, td} ->
        eq([a | d], xs)
        eq([ta | td], val)
        evalo(a, env, ta)
        proper_listo(d, env, td)
    end
  end

  def lookupo(x, env, t) do
    fresh {y, v, rest} do
      eq([[y | v] | rest], env)

      condi do
        _ ->
          eq(y, x)
          eq(v, t)

        _ ->
          neq(y, x)
          lookupo(x, rest, t)
      end
    end
  end

  # @tag :skip
  @tag timeout: 600_000
  test "code for testing" do
    for p <- run(10, q, do: evalo(q, [], q)) do
      case p do
        {t, _c} ->
          IO.puts(print(t))

        t ->
          IO.puts(print(t))
      end
    end
  end

  test "test existing quine" do
    assert [1] = run(1, q, do: evalo(:x, [[:x | 1]], q))

    quine = [
      [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]],
      [:quote, [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]]]
    ]

    assert [:_0] = run(q, do: evalo(quine, [], quine))
  end

  test "guided generation of quine" do
    quine = [
      [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]],
      [:quote, [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]]]
    ]

    assert [_ | _] = run(200, q, do: evalo(q, [], quine))
  end

  test "guided generation of quine - 2" do
    quine = [
      [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]],
      [:quote, [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]]]
    ]

    result =
      run(1, q) do
        fresh {x, y} do
          eq(q, [[:lambda, [:x], x], y])
        end

        evalo(q, [], q)
      end

    assert [quine] == result
  end

  @tag timeout: 600_000
  test "quine generation" do
    assert [_] = run(1, q, do: evalo(q, [], q))
  end

  @pretty_sym %{
    :_0 => "x",
    :_1 => "y",
    :_2 => "z",
    :_3 => "a",
    :_4 => "b",
    :_5 => "c"
  }
  @pretty_sym_keys Map.keys(@pretty_sym)

  def print(exp) do
    case exp do
      [:quote, v] ->
        ["'", print(v)]

      [:list | xs] ->
        ["(", "list", " ", Enum.map(xs, &print/1) |> Enum.join(" "), ")"]

      [:lambda, [v], body] ->
        ["(", "lambda", " (", print(v), ") ", print(body), ")"]

      [rator, rand] ->
        ["(", print(rator), " ", print(rand), ")"]

      %Var{} = v ->
        inspect(v)

      xs when is_list(xs) ->
        ["(", xs |> Enum.map(&print/1) |> Enum.join(" "), ")"]

      exp when exp in @pretty_sym_keys ->
        @pretty_sym[exp]

      sym ->
        to_string(sym)
    end
    |> :erlang.iolist_to_binary()
  end
end
