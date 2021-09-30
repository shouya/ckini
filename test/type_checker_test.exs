defmodule TypeCheckerTest do
  use ExUnit.Case
  use Ckini

  def has_typ(exp, env, typ) do
    condi do
      _ ->
        eq(exp, true)
        eq(typ, :Bool)

      _ ->
        eq(exp, false)
        eq(typ, :Bool)

      _ ->
        symbolo(exp)
        neq(exp, true)
        neq(exp, false)
        lookupo(exp, env, typ)

      {t, cond_, then_, else_} ->
        eq([:if, cond_, then_, else_], exp)
        has_typ(cond_, env, :Bool)
        has_typ(then_, env, t)
        has_typ(else_, env, t)
        eq(t, typ)

      {x, xt, body, closure_env, body_typ} ->
        eq([:lambda, [x | xt], body], exp)
        eq(closure_env, [[x | xt] | env])
        eq(typ, [:fn, xt, body_typ])
        has_typ(body, closure_env, body_typ)

      {rator, rand, rand_typ} ->
        eq([rator, rand], exp)
        neq(rator, :lambda)
        has_typ(rand, env, rand_typ)
        has_typ(rator, env, [:fn, rand_typ, typ])
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

  test "generating some valid programs" do
    pairs = run(5, {exp, t}, do: has_typ(exp, [], t))

    for {{exp, t}, _constraints} <- pairs do
      IO.puts("#{print(exp)} : #{print_t(t)}")
    end

    # Output:
    # (lambda (x:S) x) : S -> S
    # (lambda (x:S) (lambda (z:B) z)) : S -> B -> B
    # (lambda (x:S) (lambda (z:B) x)) : S -> B -> S
    # ((lambda (x:S -> S) x) (lambda (z:S) z)) : S -> S
    # (lambda (x:S) (lambda (z:B) (lambda (b:D) b))) : S -> B -> D -> D
  end

  test "examples on TaPL" do
    tX = Var.new(:X)
    tZZ = Var.new(:ZZ)
    tYY = Var.new(:YY)
    tW = Var.new(:W)

    exprs = [
      true,
      [:lambda, [:x | tX], :x],
      [:lambda, [:z | tZZ], [:lambda, [:y | tYY], [:z, [:y, true]]]],
      [:lambda, [:w | tW], [:if, true, false, [:w, false]]]
    ]

    for expr <- exprs do
      [t] = run(1, t, do: has_typ(expr, [], t))
      IO.puts("#{print(expr)} : #{print_t(t)}")
    end
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
      [:lambda, [v | t], body] ->
        ["(", "lambda", " (", print(v), ":", print_t(t), ") ", print(body), ")"]

      [rator, rand] ->
        ["(", print(rator), " ", print(rand), ")"]

      %Var{} = v ->
        to_string(v)

      xs when is_list(xs) ->
        ["(", xs |> Enum.map(&print/1) |> Enum.join(" "), ")"]

      exp when exp in @pretty_sym_keys ->
        @pretty_sym[exp]

      sym ->
        to_string(sym)
    end
    |> :erlang.iolist_to_binary()
  end

  @pretty_typ %{
    :_0 => "T",
    :_1 => "S",
    :_2 => "A",
    :_3 => "B",
    :_4 => "C",
    :_5 => "D"
  }
  @pretty_typ_keys Map.keys(@pretty_typ)

  def print_t(t) do
    case t do
      [:fn, [:fn, t1, t2], t3] ->
        "(#{print_t(t1)} -> #{print_t(t2)}) -> #{print_t(t3)}"

      [:fn, t1, t2] ->
        "#{print_t(t1)} -> #{print_t(t2)}"

      %Var{} = v ->
        to_string(v)

      t when t in @pretty_typ_keys ->
        @pretty_typ[t]

      t ->
        to_string(t)
    end
  end
end
