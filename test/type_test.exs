defmodule TypeTest do
  use ExUnit.Case
  use Ckini

  defguard is_scalar(expr) when is_atom(expr) or is_integer(expr)

  def has_typo(expr, typ, ctx) do
    condi do
      _ ->
        integero(expr)
        eq(typ, :int)

      {var} ->
        symbolo(expr)
        eq(var, expr)
        membero([var, typ], ctx)

      {x, xt, bodyt, body} ->
        eq(expr, [:fn, x, xt, body])
        symbolo(x)
        has_typo(body, bodyt, [[x, xt] | ctx])
        eq(typ, [:->, xt, bodyt])

      {f, x, a, b} ->
        eq(expr, [f, x])
        has_typo(x, a, ctx)
        has_typo(f, [:->, a, b], ctx)
        eq(typ, b)
    end
  end

  def membero(elem, list) do
    fresh {x, xs} do
      eq(list, [x | xs])

      condi do
        _ -> eq(x, elem)
        _ -> membero(elem, xs)
      end
    end
  end

  # def integero(var, from \\ 0) do
  #   condi do
  #     _ -> eq(var, from)
  #     _ -> integero(var, from + 1)
  #   end
  # end
  def integero(expr) do
    condi do
      _ -> eq(expr, 0)
      _ -> eq(expr, 1)
      _ -> eq(expr, 2)
    end
  end

  def simple_context do
    [
      [:add, [:->, :int, [:->, :int, :int]]]
    ]
  end

  @tag timeout: 60_000
  test "find values of type int" do
    goal =
      run_stream(e) do
        has_typo(e, :int, simple_context())
      end

    for e <- Stream.take(goal, 100) do
      # IO.inspect(e)
      IO.puts(format_expr(e))
    end
  end

  @spec format_expr(expr :: term()) :: binary()
  defp format_expr(expr) do
    case expr do
      x when is_integer(x) ->
        to_string(x)

      x when is_atom(x) ->
        format_var(x)

      [:fn, v, t, body] ->
        "fn (#{format_var(v)} : #{format_typ(t)}) #{format_expr(body)}"

      [f, x] when is_list(f) and is_list(x) ->
        "(#{format_expr(f)}) (#{format_expr(x)})"

      [f, x] when is_list(f) and is_scalar(x) ->
        "(#{format_expr(f)}) #{format_expr(x)}"

      [f, x] when is_scalar(f) and is_list(x) ->
        "#{format_expr(f)} (#{format_expr(x)})"

      [f, x] when is_scalar(f) and is_scalar(x) ->
        "#{format_expr(f)} #{format_expr(x)}"
    end
  end

  @spec format_var(var :: atom()) :: binary()
  defp format_var(v) do
    s = to_string(v)

    case s do
      "_0" -> "x"
      "_1" -> "y"
      "_2" -> "z"
      "_3" -> "w"
      _ -> s
    end
  end

  @spec format_typ(texpr :: term()) :: binary()
  defp format_typ(texpr) do
    case texpr do
      :int -> "Int"
      type_var when is_atom(type_var) -> format_type_var(type_var)
      [:->, a, b] when is_atom(a) -> "#{format_typ(a)} -> #{format_typ(b)}"
      [:->, a, b] -> "(#{format_typ(a)}) -> #{format_typ(b)}"
    end
  end

  @spec format_type_var(var :: atom()) :: binary()
  defp format_type_var(v) do
    s = to_string(v)

    case s do
      "_0" -> "T"
      "_1" -> "T"
      _ -> s
    end
  end
end
