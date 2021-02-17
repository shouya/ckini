defmodule QuineTest do
  use ExUnit.Case
  use Ckini

  # translated from
  # https://github.com/webyrd/Barliman/blob/master/cocoa/Barliman/mk-and-rel-interp/mk/test-quines.scm

  def evalo(exp, env, val) do
    condi([
      fn ->
        v = Var.new()

        [
          eq([:quote, v], exp),
          eq(v, val),
          not_in_envo(:quote, env),
          absento(v, :closure)
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
        [symbolo(exp), lookupo(exp, env, val)]
      end,
      fn ->
        [rator, rand, x, body, env_n, a] = Var.new_many(6)

        [
          eq([rator, rand], exp),
          evalo(rator, env, [:closure, x, body, env_n]),
          project({rator, rand}, fn {r, rr} ->
            IO.puts(print([r, rr]))
            succ()
          end),
          evalo(rand, env, a),
          evalo(body, [[x | a] | env_n], val)
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
    condi([
      fn -> [eq([], xs), eq([], val)] end,
      fn ->
        [a, d, ta, td] = Var.new_many(4)

        [
          eq([a | d], xs),
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

  def not_in_envo(x, env) do
    condi([
      fn ->
        [y, v, rest] = Var.new_many(3)

        [
          eq(env, [[y | v] | rest]),
          neq(y, x),
          not_in_envo(x, rest)
        ]
      end,
      fn -> eq(env, []) end
    ])
  end

  test "test existing quine" do
    q = Var.new()
    assert [1] = run(1, q, evalo(:x, [[:x | 1]], q))

    quine = [
      [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]],
      [:quote, [:lambda, [:x], [:list, :x, [:list, [:quote, :quote], :x]]]]
    ]

    assert [:_0] = run(q, evalo(q, [], quine))
  end

  @tag timeout: 60_000
  test "quine generation" do
    q = Var.new()

    assert [:_0] = run(1, q, evalo(q, [], q))
  end

  def print(exp) do
    case exp do
      [:quote, v] ->
        ["'", print(v)]

      [:list | xs] ->
        ["(", "list", Enum.map(xs, &print/1) |> Enum.join(" "), ")"]

      [rator, rand] ->
        ["(", print(rator), " ", print(rand), ")"]

      %Var{} = v ->
        inspect(v)

      sym ->
        to_string(sym)
    end
    |> :erlang.iolist_to_binary()
  end
end
