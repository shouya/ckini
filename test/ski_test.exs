defmodule SKITest do
  use ExUnit.Case
  use Ckini

  def ski_stepo(a, b) do
    condi do
      {x, a_} ->
        eq(a, [:I, x | a_])
        eq(b, [x | a_])

      {x, y, z, a_} ->
        eq(a, [:S, x, y, z | a_])
        eq(b, [x, z, [y, z] | a_])

      {x, y, a_} ->
        eq(a, [:K, x, y | a_])
        eq(b, [x | a_])

      {x, y, z, a_} ->
        eq(a, [[x | y] | a_])
        eq(b, [z | a_])
        ski_stepo([x | y], z)
    end
  end

  def skio(a, b) do
    condi do
      _ ->
        eq(a, b)

      c ->
        ski_stepo(a, c)
        skio(c, b)
    end
  end

  def appendo(l, s, out) do
    condi do
      _ ->
        eq(l, [])
        eq(s, out)

      {a, d, res} ->
        eq([a | d], l)
        eq([a | res], out)
        appendo(d, s, res)
    end
  end

  @tag timeout: 60_000
  test "find equivalent of 'I'" do
    goal =
      run_stream(t) do
        fresh e do
          absento(t, :I)
          absento(t, :x)
          appendo(t, [:x], e)
          skio(e, [:x])
        end
      end

    for t <- goal do
      # []
      # [:S, :K, :_0]
      # [:S, :S, :_0, :K]
      # [:K, :S, :_0, :K, :_1]
      # [:S, :S, :_0, :S, :K]
      # [:S, :K, :_0, :S, :K, :_1]
      # [:K, :S, :_0, :S, :_1, :K]
      # [:S, :S, :_0, :S, :S, :K]
      # [:K, :K, :_0, :S, :_1, :K, :_2]
      # [:S, :S, :_0, :K, :S, :K, :_1]
      # [:S, :K, :_0, :S, :S, :_1, :K]
      # [:K, :S, :_0, :S, :_1, :S, :K]
      # [:S, :S, :_0, :S, :S, :S, :K]
      IO.inspect(t)
    end
  end
end
