defmodule LambdaCubeTest do
  @moduledoc """
  Rules come from the formal definition of Lambda Cube:

  https://en.wikipedia.org/wiki/Lambda_cube#Formal_definition

  - Note: alpha equivalence is not implemented
  """

  use ExUnit.Case
  use Ckini

  @base_ctx [
    {:A, :type},
    {:B, :type}
  ]

  @stlc [{:type, :type}]
  @dept [{:type, :type}, {:type, :kind}]
  @sysf [{:type, :type}, {:kind, :type}]
  @cc [{:type, :type}, {:kind, :type}, {:type, :kind}, {:kind, :kind}]

  def sorto(s) do
    condi do
      _ -> eq(s, :type)
      _ -> eq(s, :kind)
    end
  end

  def complex_termo(t) do
    condi do
      {t1, t2} ->
        eq(t, {:app, t1, t2})

      {x, t1, t2} ->
        symbolo(x)
        eq(t, {:abs, x, t1, t2})

      {x, t1, t2} ->
        symbolo(x)
        eq(t, {:pi, x, t1, t2})
    end
  end

  def termo(t) do
    condi do
      x ->
        symbolo(x)
        eq(t, {:var, x})

      _ ->
        sorto(t)

      {t1, t2} ->
        eq(t, {:app, t1, t2})

      {x, t1, t2} ->
        symbolo(x)
        eq(t, {:abs, x, t1, t2})

      {x, t1, t2} ->
        symbolo(x)
        eq(t, {:pi, x, t1, t2})
    end
  end

  def betao(t1, t2) do
    condi do
      _ ->
        # reflexive closure
        eq(t1, t2)

      {x, a, b, c, o} ->
        # abs-app
        eq(t1, {:app, {:abs, x, a, b}, c})
        neq(t1, t2)
        eq(t2, o)
        subst_varo(b, x, c, o)

      {k, x, a, b, ao, bo} ->
        # abs/pi
        abs_or_pio(t1, k, x, a, b)
        abs_or_pio(t2, k, x, ao, bo)
        neq(t1, t2)
        betao(a, ao)
        betao(b, bo)

      t1x ->
        # transitive closure
        neq(t1, t2)
        neq(t1, t1x)
        neq(t1x, t2)
        betao(t1, t1x)
        betao(t1x, t2)
    end
  end

  def abs_or_pio(t, k, x, a, b) do
    all do
      symbolo(x)
      eq(t, {k, x, a, b})

      # Hack: I can save some branching calculation because only abs
      # and pi are 4-tuples.

      # condi do
      #   _ -> eq(k, :abs)
      #   _ -> eq(k, :pi)
      # end
    end
  end

  def subst_varo(a, x, b, o) do
    condi do
      y ->
        eq(a, {:var, y})

        condi do
          _ ->
            eq(x, y)
            eq(b, o)

          _ ->
            neq(x, y)
            eq(a, o)
        end

      _ ->
        eq(a, o)
        sorto(a)

      {t1, t2, t1o, t2o} ->
        eq(a, {:app, t1, t2})
        eq(o, {:app, t1o, t2o})
        subst_varo(t1, x, b, t1o)
        subst_varo(t2, x, b, t2o)

      {k, y, t1, t2, t1o, t2o} ->
        abs_or_pio(a, k, y, t1, t2)
        abs_or_pio(o, k, y, t1o, t2o)

        condi do
          _ ->
            eq(x, y)
            eq(t1o, t2o)

          t1o, t2o ->
            neq(x, y)
            subst_varo(t1, x, b, t1o)
            subst_varo(t2, x, b, t2o)
        end
    end
  end

  def has_typeo(sys, ctx, t, ty) do
    all do
      peek({t, ty})

      condi do
        _ ->
          # axiom
          eq(:type, t)
          eq(:kind, ty)

        {s, x} ->
          # var lookup
          eq({:var, x}, t)
          lookupo(ctx, x, ty)
          has_typeo(sys, ctx, ty, s)
          sorto(s)

        {rator, rand, x, a, b} ->
          # app
          eq({:app, rator, rand}, t)
          has_typeo(sys, ctx, rator, {:pi, x, a, b})
          has_typeo(sys, ctx, rand, a)
          subst_varo(b, x, rand, ty)

        {x, a, b, s1} ->
          # product
          eq({:pi, x, a, b}, t)
          symbolo(x)
          has_typeo(sys, ctx, a, s1)
          has_typeo(sys, [{x, a} | ctx], b, ty)
          systemo(sys, s1, ty)

        {x, b, ta, tb, s1, s2} ->
          # abs
          eq({:abs, x, ta, b}, t)
          eq({:pi, x, ta, tb}, ty)
          symbolo(x)
          has_typeo(sys, [{x, ta} | ctx], b, tb)
          has_typeo(sys, ctx, ta, s1)
          has_typeo(sys, [{x, ta} | ctx], tb, s2)
          systemo(sys, s1, s2)

          # {tyx, s, t1, t2} ->
          #   # conversion
          #   # to speed things up
          #   neq(:kind, ty)
          #   neq(:type, ty)
          #   eq({:app, t1, t2}, t)

          #
          # neq(ty, tyx)
          # betao(tyx, ty)
          # has_typeo(sys, ctx, t, tyx)
          # has_typeo(sys, ctx, ty, s)
          # sorto(s)
      end
    end
  end

  def lookupo(ctx, x, ty) do
    fresh {y, ctx_, ty_} do
      symbolo(x)
      eq([{y, ty_} | ctx_], ctx)

      condi do
        _ ->
          eq(x, y)
          eq(ty, ty_)

        _ ->
          neq(x, y)
          lookupo(ctx_, x, ty)
      end
    end
  end

  def systemo(sys, s1, s2) do
    fresh {s, sys_} do
      eq([s | sys_], sys)

      condi do
        _ ->
          eq({s1, s2}, s)

        _ ->
          neq({s1, s2}, s)
          systemo(sys_, s1, s2)
      end
    end
  end

  @tag timeout: 60_000
  test "test" do
    _exp = [
      # stlc, env: @base_ctx
      {:abs, :x, {:var, :A}, {:var, :x}},
      # sysf
      {:abs, :A, :type, {:abs, :x, {:var, :A}, {:var, :x}}}
      # dept, env: @base_ctx
    ]

    # Type  :  Kind
    # A  :  Type
    # Type -> Type  :  Kind
    # A -> Type  :  Kind
    # (x:Type) -> x  :  Type
    # B  :  Type
    # (x:Type), x  :  Type -> Type
    # Type -> Type -> Type  :  Kind
    # ((x:Type), x) A  :  Type
    # (Type -> Type) -> Type  :  Kind
    # A -> Type -> Type  :  Kind
    # ((x:Type), x) ((x:Type) -> x)  :  Type
    # (x:Type) -> x -> Type  :  Kind
    # ((x:Type), x) B  :  Type
    # Type -> (x:Type) -> x  :  Type
    # (Type -> Type) -> Type -> Type  :  Kind
    # A -> (x:Type) -> x  :  Type
    # Type -> A  :  Type
    # (A -> Type) -> Type  :  Kind
    # ((x:Type) -> x) -> Type  :  Kind
    # (Type -> Type) -> (x:Type) -> x  :  Type
    # ((x:Type), x) ((x:Type), x) A  :  Type
    # Type -> Type -> Type -> Type  :  Kind
    # (A -> Type) -> Type -> Type  :  Kind
    # A -> A  :  Type
    # ((x:Type), x) ((x:Type), x) ((x:Type) -> x)  :  Type
    # ((x:Type), x) (Type -> (x:Type) -> x)  :  Type
    # ((x:Type), x) (A -> (x:Type) -> x)  :  Type
    # ((x:Type) -> x) -> Type -> Type  :  Kind
    # Type -> (Type -> Type) -> Type  :  Kind
    # A -> Type -> Type -> Type  :  Kind
    # (x:Type) -> ((x:Type), x) x  :  Type
    # ((x:Type), x) ((x:Type), x) B  :  Type
    # A -> (Type -> Type) -> Type  :  Kind
    # (x:Type) -> x -> Type -> Type  :  Kind
    # (A -> Type) -> (x:Type) -> x  :  Type

    for {t, ty} <- run_stream({t, ty}, do: has_typeo(@cc, @base_ctx, t, ty)) do
      IO.puts("#{print(t)}  :  #{print(ty)}")
    end
  end

  @tag timeout: 120_000
  test "find nat" do
    nat =
      {:pi, :a, :type,
       {:pi, :z, {:var, :a},
        {:pi, :s, {:pi, :x, {:var, :a}, {:var, :a}}, {:var, :a}}}}

    IO.puts("Nat = #{print(nat)}")

    for t <- run(t, limit: 1, do: has_typeo(@cc, [], nat, t)) do
      # Prints:
      # Nat : Type
      IO.puts("Nat : #{print(t)}")
    end

    for t <- run_stream(t, do: has_typeo(@cc, [], t, nat)) do
      # Prints:
      # (a:Type) => (z:a) => (s:a -> a) => z : Nat
      # (a:Type) => (z:a) => (s:a -> a) => s z : Nat
      # (a:Type) => (z:a) => (s:a -> a) => s s z : Nat
      IO.puts("#{print(t)} : Nat")
    end
  end

  test "print" do
    x = {:var, :x}
    app = fn a, b -> {:app, a, b} end
    abs = fn x, a, b -> {:abs, x, a, b} end

    assert print(app.(app.(x, x), x)) == "(x x) x"
    assert print(abs.(:x, x, abs.(:x, x, x))) == "(x:x) => (x:x) => x"
    assert print(abs.(:x, abs.(:x, x, x), x)) == "(x:(x:x) => x) => x"
    assert print(app.(abs.(:x, x, x), x)) == "((x:x) => x) x"
    assert print(abs.(:x, x, app.(x, x))) == "(x:x) => x x"
  end

  def print(ty, prec \\ 5) do
    case ty do
      {:var, x} ->
        to_string(x)

      {:pi, x, a, b} ->
        # prec: 3, right associativity
        if has_var?(b, x) do
          paren(prec < 3, "(#{x}:#{print(a, 3)}) -> #{print(b, 3)}")
        else
          paren(prec < 3, "#{print(a, 2)} -> #{print(b, 3)}")
        end

      {:abs, x, a, b} ->
        # prec 3, right associativity
        paren(prec < 3, "(#{x}:#{print(a, 3)}) => #{print(b, 3)}")

      {:app, a, b} ->
        # prec 2, right associativity
        paren(prec < 2, "#{print(a, 1)} #{print(b, 2)}")

      :type ->
        "Type"

      :kind ->
        "Kind"
    end
  end

  defp has_var?(t, x) do
    case t do
      {:var, ^x} -> true
      {:var, _} -> false
      {:pi, ^x, _a, _b} -> false
      {:abs, ^x, _a, _b} -> false
      {:pi, _, a, b} -> has_var?(a, x) or has_var?(b, x)
      {:abs, _, a, b} -> has_var?(a, x) or has_var?(b, x)
      {:app, a, b} -> has_var?(a, x) or has_var?(b, x)
      :type -> false
      :kind -> false
    end
  end

  defp paren(show?, str) do
    if show?, do: "(#{str})", else: str
  end
end
