defmodule QuineTest do
  use ExUnit.Case
  use Ckini

  # translated from
  # https://github.com/webyrd/Barliman/blob/master/cocoa/Barliman/mk-and-rel-interp/mk/test-quines.scm

  def evalo(exp, env, val) do
    condi([
      fn ->
        v = Var.new()
        [eq([:quote, v], exp), eq(v, val)]
      end,
      fn ->
        xs = Var.new()
        [eq([:list | xs], exp), proper_listo(xs, env, val)]
      end,
      fn ->
        [varo(exp), lookupo(exp, env, val)]
      end,
      fn ->
        [rator, rand, x, body, env_n, a] = Var.new_many(6)

        [
          eq([rator, rand], exp),
          evalo(rator, env, [:closure, x, body, env_n]),
          evalo(rand, env, a),
          evalo(body, [[x | a] | env_n], val)
        ]
      end,
      fn ->
        [x, body] = Var.new_many(2)

        [
          eq([:lambda, [x], body], exp),
          varo(x),
          eq([:closure, x, body, env], val)
        ]
      end
    ])
  end

  def proper_listo(exp, env, val) do
    condi([
      fn -> [eq([], exp), eq([], val)] end,
      fn ->
        [a, d, ta, td] = Var.new_many(4)

        [
          eq([a | d], exp),
          eq([ta | td], val),
          evalo(a, env, ta),
          proper_listo(d, env, td)
        ]
      end
    ])
  end

  def lookupo(x, env, t) do
    [y, v, rest] = Var.new_many(3)

    all([
      eq([[y | v] | rest], env),
      condi([
        fn -> [eq(y, x), eq(v, t)] end,
        fn -> [neq(y, x), lookupo(x, rest, t)] end
      ])
    ])
  end

  def varo(v) do
    condi([eq(v, :x), eq(v, :quote), eq(v, :list)])
  end

  @tag timeout: 100_000
  test "quine" do
    q = Var.new()
    env = [[:list | :list], [:quote | :quote]]

    assert [1] = run(1, q, evalo(:x, [[:x | 1]], q))

    quine = [
      [:lambda, [:x], [:list, :x, [:list, :quote, :x]]],
      [:quote, [:lambda, [:x], [:list, :x, [:list, :quote, :x]]]]
    ]

    assert [quine] == run(q, evalo(quine, env, q))
  end
end
