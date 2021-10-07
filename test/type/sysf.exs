defmodule TypeChecker.STLCTest do
  use ExUnit.Case
  use Ckini

  @base_typs [:Bool]

  def has_typ_closed(exp, typ) do
    has_typ(exp, [], @base_typs, typ)
  end

  def has_typ(exp, env, typ_env, typ) do
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
        has_typ(cond_, env, typ_env, :Bool)
        has_typ(then_, env, typ_env, t)
        has_typ(else_, env, typ_env, t)
        eq(t, typ)

      # T-TAbs
      {typ_var, body, body_typ, new_typ_env} ->
        eq([:Lam, [typ_var], body], exp)
        eq([:forall, typ_var, body_typ], typ)
        eq(new_typ_env, [typ_var | typ_env])
        symbolo(typ_var)
        not_in_listo(typ_var, typ_env)
        has_typ(body, env, new_typ_env, body_typ)

      # T-TApp
      {term, typ_var, typ_arg, abs_typ} ->
        eq([:TApp, term, typ_arg], exp)
        has_typ(term, env, typ_env, [:forall, typ_var, abs_typ])
        subst_typ(abs_typ, typ_var, typ_arg, typ)

      # T-Abs
      {x, xt, body, closure_env, body_typ} ->
        eq([:lambda, [x | xt], body], exp)
        eq([:fn, xt, body_typ], typ)
        eq(closure_env, [[x | xt] | env])
        in_listo(xt, typ_env)
        has_typ(body, closure_env, typ_env, body_typ)

      # T-App
      {rator, rand, rand_typ} ->
        eq([rator, rand], exp)
        has_typ(rator, env, typ_env, [:fn, rand_typ, typ])
        has_typ(rand, env, typ_env, rand_typ)
    end
  end

  def subst_typ(typ, from, to, out_typ) do
    condi do
      _ ->
        symbolo(typ)
        eq(typ, from)
        eq(out_typ, to)

      _ ->
        symbolo(typ)
        neq(typ, from)
        eq(out_typ, typ)

      {a, b, new_a, new_b} ->
        eq(typ, [:fn, a, b])
        eq(out_typ, [:fn, new_a, new_b])
        subst_typ(a, from, to, new_a)
        subst_typ(b, from, to, new_b)

      {t} ->
        eq(typ, [:forall, from, t])
        eq(out_typ, [:forall, from, t])

      {x, new_t, t} ->
        neq(x, from)
        eq(typ, [:forall, x, t])
        eq(out_typ, [:forall, x, new_t])
        subst_typ(t, from, to, new_t)
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

  def in_listo(x, list) do
    fresh {y, rest} do
      eq([y | rest], list)

      condi do
        _ ->
          eq(y, x)

        _ ->
          neq(y, x)
          in_listo(x, rest)
      end
    end
  end

  def not_in_listo(x, list) do
    condi do
      _ ->
        eq(list, [])

      {y, rest} ->
        eq(list, [y | rest])
        neq(x, y)
        not_in_listo(x, rest)
    end
  end

  test "generating some valid programs" do
    pairs = run(50, {exp, t}, do: has_typ_closed(exp, t))

    for pair <- pairs do
      case pair do
        {{exp, t}, _constraints} ->
          IO.puts("#{print(exp)} : #{print_t(t)}")

        {exp, t} ->
          IO.puts("(#{print(exp)}) : #{print_t(t)}")
      end
    end

    # Output:
    # (true) : Bool
    # (false) : Bool
    # Λ T. true : forall T. Bool
    # ((if true true true)) : Bool
    # ((if true true false)) : Bool
    # Λ T. false : forall T. Bool
    # ((if true false true)) : Bool
    # (λ (x:S). true) : S -> Bool
    # ((if true false false)) : Bool
    # ((if false true true)) : Bool
    # Λ T. Λ S. true : forall T. forall S. Bool
    # Λ T. (if true true true) : forall T. Bool
    # (λ (x:S). false) : S -> Bool
    # ((if true true (if true true true))) : Bool
    # ((if false true false)) : Bool
    # Λ T. (if true true false) : forall T. Bool
    # Λ T. Λ S. false : forall T. forall S. Bool
    # ((if true true (if true true false))) : Bool
    # (TApp Λ T. true y) : Bool
  end

  test "identity function" do
    exp = [:Lam, [:X], [:lambda, [:a | :X], :a]]
    [t] = run(1, t, do: has_typ_closed(exp, t))

    IO.puts(print(exp))
    IO.puts(print_t(t))
  end

  test "parametricity demo" do
    t = [:forall, :X, [:fn, :X, [:fn, :X, :X]]]

    IO.puts(print_t(t))

    for exp <- run(2, exp, do: has_typ_closed(exp, t)) do
      case exp do
        {exp, _constr} ->
          IO.puts(print(exp))

        exp ->
          IO.puts(print(exp))
      end
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
        ["λ", " (", print(v), ":", print_t(t), "). ", print(body)]

      [:Lam, [t], body] ->
        ["Λ", " ", print_t(t), ". ", print(body)]

      [:TApp, a, t] ->
        [print(a), " [", print_t(t), "]"]

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

      [:forall, a, t1] ->
        "forall #{print_t(a)}. #{print_t(t1)}"

      %Var{} = v ->
        to_string(v)

      t when t in @pretty_typ_keys ->
        @pretty_typ[t]

      t ->
        to_string(t)
    end
  end
end
