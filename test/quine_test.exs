defmodule QuineTest do
  use ExUnit.Case
  use Ckini

  def evalo(exp, env, val) do
    condem([
      fn ->
        v = Var.new()

        [
          eq([:quote, v], exp),
          not_in_envo(:quote, env),
          absento(v, :closure),
          eq(v, val)
        ]
      end,
      fn ->
        xs = Var.new()

        [
          eq([:list | xs], exp),
          not_in_envo(:list, env),
          absento(xs, :closure),
          proper_listo(xs, env, val)
        ]
      end,
      fn ->
        [
          symbolo(exp),
          lookupo(exp, env, val)
        ]
      end,
      fn ->
        [rator, rand, x, body, env_n, a] = Var.new_many(6)

        [
          eq([rator, rand], exp),
          fn -> evalo(rator, env, [:closure, x, body, env_n]) end,
          fn -> evalo(rand, env, a) end,
          fn -> evalo(body, [[x | a] | env_n], val) end
        ]
      end,
      fn ->
        [x, body] = Var.new_many(2)

        [
          eq([:lambda, [x], body], exp),
          symbolo(x),
          not_in_envo(:lambda, env),
          eq([:closure, x, body, env], val)
        ]
      end
    ])
  end

  def proper_listo(xs, env, val) do
    condem([
      fn -> [eq([], xs), eq([], val)] end,
      fn ->
        [a, d, ta, td] = Var.new_many(4)

        [
          eq([a | d], xs),
          eq([ta | td], val),
          fn -> evalo(a, env, ta) end,
          proper_listo(d, env, td)
        ]
      end
    ])
  end

  def lookupo(x, env, t) do
    [y, v, rest] = Var.new_many(3)

    all([
      eq([[y | v] | rest], env),
      conde([
        fn -> [eq(y, x), eq(v, t)] end,
        fn -> [neq(y, x), lookupo(x, rest, t)] end
      ])
    ])
  end

  def not_in_envo(x, env) do
    conde([
      fn ->
        [y, v, rest] = Var.new_many(3)

        [
          eq(env, [[y | v] | rest]),
          neq(y, x),
          fn -> not_in_envo(x, rest) end
        ]
      end,
      fn -> eq(env, []) end
    ])
  end

  @tag :skip
  @tag timeout: 600_000
  test "code for testing" do
    [q] = Var.new_many(1)

    for p <- run(2, q, evalo(q, [], q)) do
      case p do
        {t, _c} ->
          IO.puts(print(t))

        t ->
          IO.puts(print(t))
      end
    end
  end

  test "test existing quine" do
    q = Var.new()
    assert [1] = run(1, q, evalo(:x, [[:x | 1]], q))

    quine = [
      [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]],
      [:quote, [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]]]
    ]

    assert [:_0] = run(q, evalo(quine, [], quine))
  end

  test "guided generation of quine" do
    q = Var.new()

    quine = [
      [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]],
      [:quote, [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]]]
    ]

    assert [_ | _] = run(200, q, evalo(q, [], quine))
  end

  test "guided generation of quine - 2" do
    q = Var.new()

    quine = [
      [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]],
      [:quote, [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]]]
    ]

    [x, y, _z] = Var.new_many(3)

    assert [quine] ==
             run(1, q, [eq(q, [[:lambda, [:x], x], y]), evalo(q, [], q)])
  end

  @tag timeout: 600_000
  test "quine generation" do
    q = Var.new()

    assert [_] = run(1, q, evalo(q, [], q))
  end

  def print(exp) do
    case exp do
      [:quote, v] ->
        ["'", print(v)]

      [:list | xs] ->
        ["(", "list", " ", Enum.map(xs, &print/1) |> Enum.join(" "), ")"]

      [:lambda, [v], body] ->
        ["(", "lambda", " (", to_string(v), ") ", print(body), ")"]

      [rator, rand] ->
        ["(", print(rator), " ", print(rand), ")"]

      %Var{} = v ->
        inspect(v)

      xs when is_list(xs) ->
        ["(", xs |> Enum.map(&print/1) |> Enum.join(" "), ")"]

      sym ->
        to_string(sym)
    end
    |> :erlang.iolist_to_binary()
  end
end
