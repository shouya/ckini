defmodule Ckini.Goals do
  @moduledoc """
  This module defines generic goals and combinators like anyo.
  """

  alias Ckini.{Stream, Subst, Var}
  import Ckini

  @type goal :: Ckini.goal()

  @doc """
  succ is a goal that always succeeds.

  iex> use Ckini
  iex> x = Var.new()
  iex> run(x, [succ(), eq(x, 1)])
  [1]
  """
  @spec succ :: goal()
  def succ do
    fn s -> Stream.singleton(s) end
  end

  @spec fail :: goal()
  def fail do
    fn _ -> Stream.empty() end
  end

  @spec eq(Term.t(), Term.t()) :: goal()
  def eq(v, w) do
    fn s ->
      case unify(v, w, s) do
        :fail -> Stream.empty()
        new_s -> Stream.singleton(new_s)
      end
    end
  end

  @spec neq(Term.t(), Term.t()) :: goal()
  def neq(v, w) do
    fn s ->
      case unify(v, w, s) do
        :fail -> Stream.singleton(s)
        _s -> Stream.empty()
      end
    end
  end

  @doc """
  Anyo runs the goal for indefinitely number of times.

  iex> import Ckini
  iex> x = Ckini.Var.new()
  iex> run(5, x, anyo(eq(x, 1)))
  [1, 1, 1, 1, 1]
  """
  def anyo(g) do
    conde([g, fn -> anyo(g) end])
  end

  @doc """
  onceo succeeds at most once.

  iex> import Ckini
  iex> x = Ckini.Var.new()
  iex> run(x, onceo(anyo(eq(x, 1))))
  [1]
  """
  def onceo(g) do
    condu([[g]])
  end

  @doc """

  copy_termo creates a copy of its first argument, replacing logic
  variables with new variables.

  iex> import Ckini
  iex> w = Ckini.Var.new()
  iex> x = Ckini.Var.new()
  iex> y = Ckini.Var.new()
  iex> z = Ckini.Var.new()
  iex> run({w, z}, [
  ...>   eq([:a, x, 5, y, x], w),
  ...>   copy_termo(w, z)
  ...> ])
  [{[:a, :_0, 5, :_1, :_0],
    [:a, :_2, 5, :_3, :_2]}]
  """
  def copy_termo(t1, t2) do
    project(t1, fn t ->
      subs = build_s(Subst.new(), t)
      eq(t2, Subst.deep_walk(subs, t))
    end)
  end

  defp build_s(subs, t) do
    case t do
      %Var{} = v -> Subst.insert(subs, v, Var.clone(v))
      [t | ts] -> subs |> build_s(t) |> build_s(ts)
      _ -> subs
    end
  end
end
