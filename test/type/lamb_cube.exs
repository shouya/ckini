defmodule LambdaCubeTest do
  @moduledoc """
  Rules come from the formal definition of Lambda Cube:

  https://en.wikipedia.org/wiki/Lambda_cube#Formal_definition
  """

  use ExUnit.Case
  use Ckini

  @base_ctx [{:A, :type}, {:B, :type}]

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
        neq(t1, t2)
        eq(t1, {:app, {:abs, x, a, b}, c})
        subst_varo(b, x, c, o)
        eq(t2, o)

      {k, x, a, b, ao, bo} ->
        # abs/pi
        neq(t1, t2)
        abs_or_pio(t1, k, x, a, b)
        abs_or_pio(t2, k, x, ao, bo)
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
      eq(t, {k, x, a, b})

      condi do
        _ -> eq(k, :abs)
        _ -> eq(k, :pi)
      end
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
        sorto(a)
        eq(a, o)

      {t1, t2, t1o, t2o} ->
        eq(a, {:app, t1, t2})
        subst_varo(t1, x, b, t1o)
        subst_varo(t2, x, b, t2o)
        eq(o, {:app, t1o, t2o})

      {k, y, t1, t2} ->
        abs_or_pio(a, k, y, t1, t2)

        condi do
          _ ->
            eq(x, y)
            eq(a, o)

          t1o, t2o ->
            neq(x, y)
            subst_varo(t1, x, b, t1o)
            subst_varo(t2, x, b, t2o)
            abs_or_pio(o, k, y, t1o, t2o)
        end
    end
  end

  def has_typeo(sys, ctx, t, ty) do
    condi do
      _ ->
        # axiom
        eq(t, :type)
        eq(ty, :kind)

      {s, x, ctx_} ->
        # start
        eq({:var, x}, t)
        eq([{x, ty} | ctx_], ctx)
        not_in_ctxo(x, ctx_)
        sorto(s)
        has_typeo(sys, ctx, ty, s)

      {rator, rand, x, a, b, bb} ->
        # app
        eq(t, {:app, rator, rand})
        eq(ty, bb)
        has_typeo(sys, ctx, rator, {:pi, x, a, b})
        subst_varo(b, x, rand, bb)
        has_typeo(sys, ctx, rand, a)

      {x, a, b, s1, s2} ->
        # product
        eq({:pi, x, a, b}, t)
        eq(ty, s2)
        systemo(sys, s1, s2)
        has_typeo(sys, ctx, a, s1)
        has_typeo(sys, [{x, a} | ctx], b, s2)

      {x, b, ta, tb, s1, s2} ->
        # abs
        eq({:abs, x, ta, b}, t)
        eq({:pi, x, ta, tb}, ty)
        has_typeo(sys, [{x, ta} | ctx], b, tb)
        has_typeo(sys, ctx, ta, s1)
        systemo(sys, s1, s2)
        has_typeo(sys, [{x, ta} | ctx], tb, s2)

      {ctx_, x, a, s} ->
        # weakening
        eq([{x, a} | ctx_], ctx)
        termo(t)
        sorto(s)
        has_typeo(sys, ctx, ty, s)
        has_typeo(sys, ctx_, t, ty)

      {tyx, s} ->
        # conversion
        termo(t)
        sorto(s)
        neq(ty, tyx)
        has_typeo(sys, ctx, ty, s)
        has_typeo(sys, ctx, t, tyx)
        betao(tyx, ty)
    end
  end

  def not_in_ctxo(x, ctx) do
    condi do
      _ ->
        eq([], ctx)

      {y, a, rest} ->
        eq([{y, a} | rest], ctx)
        neq(x, y)
        not_in_ctxo(x, rest)
    end
  end

  def systemo(sys, s1, s2) do
    fresh {s, sys_} do
      eq([s | sys_], sys)

      conde do
        _ ->
          neq({s1, s2}, s)
          systemo(sys_, s1, s2)

        _ ->
          eq({s1, s2}, s)
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
    # B  :  Type
    # (x:A) -> (x:Type) -> x  :  Type
    # (x:A) -> A  :  Type
    # (x:Type), x  :  (x:Type) -> Type
    # ((x:Type), x) (A)  :  Type
    # ((x:Type), x) ((x:Type) -> x)  :  Type
    # ((x:Type), x) (B)  :  Type
    # (x:(x:Type) -> x) -> (x:Type) -> x  :  Type
    # (x:(x:Type) -> x) -> A  :  Type
    # (x:B) -> (x:Type) -> x  :  Type
    # (x:B) -> A  :  Type
    # (x:A) -> B  :  Type
    # (x:A) -> ((x:Type), x) ((x:Type) -> x)  :  Type
    # (x:A) -> ((x:Type), x) (A)  :  Type
    # ((x:Type), x) (((x:Type), x) (A))  :  Type
    # (x:A) -> (x:Type) -> (x:Type) -> x  :  Type
    # ((x:Type), x) ((x:Type) -> (x:Type) -> x)  :  Type
    # ((x:Type), x) ((x:A) -> (x:Type) -> x)  :  Type
    # (x:A) -> (x:(x:Type) -> Type) -> (x:Type) -> x  :  Type
    # (x:(x:Type) -> x) -> B  :  Type
    # ((x:Type), x) (((x:Type), x) ((x:Type) -> x))  :  Type
    # ((x:Type), x) ((x:Type) -> A)  :  Type
    # ((x:Type), x) ((x:A) -> A)  :  Type
    # ((x:Type), x) (((x:Type), x) (B))  :  Type

    for {t, ty} <- run_stream({t, ty}, do: has_typeo(@cc, @base_ctx, t, ty)) do
      IO.puts("#{print(t)}  :  #{print(ty)}")
    end
  end

  def print(ty) do
    case ty do
      {:var, x} -> to_string(x)
      {:pi, x, a, b} -> "(#{x}:#{print(a)}) -> #{print(b)}"
      {:abs, x, a, b} -> "\(#{x}:#{print(a)}), #{print(b)}"
      {:app, a, b} -> "(#{print(a)}) (#{print(b)})"
      :type -> "Type"
      :kind -> "Kind"
    end
  end
end
