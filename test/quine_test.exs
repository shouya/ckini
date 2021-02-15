defmodule QuineTest do
  use ExUnit.Case
  use Ckini

  @vars [:x, :y]

  def evalo(e, a, n \\ 3)
  def evalo(e, a, 0), do: eq(e, a)

  def evalo(e, a, n) do
    condi([
      fn ->
        [is_varo(e), eq(e, a)]
      end,
      fn ->
        [v, body] = Var.new_many(2)

        [
          is_lambdao(e, v, body),
          is_lambdao(a, v, body)
        ]
      end,
      fn ->
        [e1, e2, e1a, e2a] = Var.new_many(4)

        [
          is_applo(e, e1, e2),
          evalo(e1, e1a, n - 1),
          evalo(e2, e2a, n - 1),
          project({e1a, e2a}, fn {e1a, e2a} ->
            IO.inspect(from_e([:appl, e1a, e2a]))
            succ()
          end),
          applo(e1a, e2a, a, n)
        ]
      end
    ])
  end

  def is_expro(e) do
    condi([
      fn -> is_varo(e) end,
      fn -> is_lambdao(e, Var.new(), Var.new()) end,
      fn -> is_applo(e, Var.new(), Var.new()) end
    ])
  end

  def is_varo(e) do
    condi(
      Enum.map(@vars, fn v ->
        fn -> eq(e, v) end
      end)
    )
  end

  def is_lambdao(e, v, body) do
    [eq(e, [:lambda, v, body]), is_varo(v), is_expro(body)]
  end

  def is_applo(e, e1, e2) do
    [eq(e, [:appl, e1, e2]), is_expro(e1), is_expro(e2)]
  end

  def non_lambdao(a) do
    [e1, e2] = Var.new_many(2)

    condi([
      fn -> is_varo(a) end,
      fn -> eq(a, [:appl, e1, e2]) end
    ])
  end

  def applo(a1, a2, a, n) do
    condi([
      fn ->
        [v, body, out] = Var.new_many(3)

        [
          is_lambdao(a1, v, body),
          substo(body, v, a2, out),
          evalo(out, a, n - 1)
        ]
      end,
      fn ->
        [
          non_lambdao(a1),
          is_applo(a, a1, a2)
        ]
      end
    ])
  end

  def substo(body, v, e, out) do
    condi([
      fn -> [is_varo(body), subst_varo(body, v, e, out)] end,
      fn ->
        [v2, b, bb] = Var.new_many(3)

        [
          is_lambdao(body, v2, b),
          is_lambdao(out, v2, bb),
          substo(b, v, e, bb)
        ]
      end,
      fn ->
        [e1, e2, e1s, e2s] = Var.new_many(4)

        [
          is_applo(body, e1, e2),
          is_applo(out, e1s, e2s),
          substo(e1, v, e, e1s),
          substo(e2, v, e, e2s)
        ]
      end
    ])
  end

  def subst_varo(body, v, e, out) do
    condi([
      fn -> [neq(body, v), eq(body, out)] end,
      fn -> [eq(body, v), eq(e, out)] end
    ])
  end

  def quine(q, n) do
    evalo(q, q, n)
  end

  test "basic evalo works" do
    q = Var.new()

    assert [[:y, :y]] ==
             run(q, evalo(to_e([lambda(:x, [:x, :y]), :y]), q))
             |> all_from_e()

    assert [[lambda(:x, [:x, :x]), lambda(:x, [:x, :x])]] ==
             run(
               q,
               evalo(to_e([lambda(:x, [:x, :x]), lambda(:x, [:x, :x])]), q)
             )
             |> all_from_e()

    q1 = to_e([lambda(:x, [:x, :x]), lambda(:x, [:x, :x])])
    assert [:_0] = run(q, quine(q1, 1))
  end

  @tag :skip
  @tag timeout: 100_000
  test "quine" do
    q = Var.new()

    assert [] =
             run(1, q, [
               eq(q, [
                 :appl,
                 [:lambda, :x, [:appl, Var.new(), Var.new()]],
                 Var.new()
               ]),
               quine(q, 1)
             ])
  end

  def to_e(e) do
    case e do
      {:lambda, v, e} -> [:lambda, v, to_e(e)]
      [v, e] -> [:appl, to_e(v), to_e(e)]
      v -> v
    end
  end

  def all_from_e(xs) do
    Enum.map(xs, &from_e/1)
  end

  def from_e(e) do
    case e do
      [:lambda, v, body] -> lambda(v, from_e(body))
      [:appl, v, e] -> [from_e(v), from_e(e)]
      v -> v
    end
  end

  def lambda(v, body) do
    {:lambda, v, body}
  end
end
