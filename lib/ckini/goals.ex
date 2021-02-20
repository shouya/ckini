defmodule Ckini.Goals do
  @moduledoc """
  This module defines generic goals and combinators like anyo.
  """

  import Ckini.Functional, only: [project: 2]
  import Ckini.Macro
  alias Ckini.{Stream, Subst, Var, Context, Term}
  require Term

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

  @doc """
  succ is a goal that always succeeds.

  iex> use Ckini
  iex> x = Var.new()
  iex> run(x, conde([[fail(), eq(x, 1)], [succ(), eq(x, 2)]]))
  [2]
  """
  @spec fail :: goal()
  def fail do
    fn _ -> Stream.empty() end
  end

  @doc """
  eq is the same as the `===` operator in TRS and other miniKanren literature.

  iex> use Ckini
  iex> x = Var.new()
  iex> run(x, eq(3, x))
  [3]
  iex> run(x, eq([x], x))
  []
  """
  @spec eq(Term.t(), Term.t()) :: goal()
  def eq(v, w) do
    fn c ->
      case Context.unify(c, v, w) do
        nil -> Stream.empty()
        new_c -> Stream.singleton(new_c)
      end
    end
  end

  @doc """
  The `=/=` operator.

  iex> use Ckini
  iex> x = Var.new()
  iex> run(x, [neq(3, x), eq(x, 3)])
  []
  iex> run(x, [eq(3, x), neq(x, 3)])
  []
  iex> run(x, [eq(4, x), neq(x, 3)])
  [4]
  iex> [y,z] = Var.new_many(2)
  iex> run(x, [neq(x, [y,z]), eq(y, 999), eq(z, 998)])
  [{:_0, [neq: [[_0: [999, 998]]]]}]
  iex> run(x, [neq(x, [:hello, :world]), neq(x, :hello)])
  [{:_0, [neq: [[_0: :hello], [_0: [:hello, :world]]]]}]
  iex> run(x, [neq(x, [y,z]), neq(x, :hello), eq(x, [1,2,3])])
  [[1,2,3]]
  """
  def neq(v, w) do
    fn c ->
      case Context.disunify(c, v, w) do
        nil -> Stream.empty()
        new_c -> Stream.singleton(new_c)
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
    condi do
      _ -> g
      _ -> anyo(g)
    end
  end

  @doc """
  onceo succeeds at most once.

  iex> import Ckini
  iex> x = Ckini.Var.new()
  iex> run(x, onceo(anyo(eq(x, 1))))
  [1]
  """
  def onceo(g) do
    condu do
      _ -> g
    end
  end

  @doc """
  copy_termo creates a copy of its first argument, replacing logic
  variables with new variables.

  iex> use Ckini
  iex> [w, x, y, z] = Var.new_many(4)
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

  @doc """
  Assert a variable to be a symbol.

  iex> use Ckini
  iex> q = Var.new()
  iex> run(q, [eq(q, :hello), symbolo(q)])
  [:hello]
  iex> run(q, [eq(q, []), symbolo(q)])
  []
  iex> run(q, [symbolo(q), conde([eq(q, 1), eq(q, :a), eq(q, [])])])
  [:a]
  iex> v = Var.new()
  iex> run(q, [symbolo(q), symbolo(v)])
  [{:_0, sym: [:_0]}]
  """
  @spec symbolo(Term.t()) :: goal()
  def symbolo(v) do
    fn c ->
      t = Subst.walk(c.subst, v)

      case Context.add_sym_constraint(c, v, t) do
        nil -> Stream.empty()
        c -> Stream.singleton(c)
      end
    end
  end

  @doc """
  Assert term t never contains term u.

  iex> use Ckini
  iex> q = Var.new()
  iex> run(q, absento(q, 42))
  [{:_0, [abs: [_0: 42]]}]
  iex> run(q, [eq(q, 42), absento(q, 42)])
  []
  iex> run(q, [eq(q, [:the, :answer, :is, [42]]), absento(q, 42)])
  []
  """
  @spec absento(Term.t(), Term.t()) :: goal()
  def absento(t, u) do
    fn c ->
      if Term.contains?(t, u, c.subst),
        do: Stream.empty(),
        else: Stream.singleton(Context.add_abs_constraint(c, t, u))
    end
  end

  defp build_s(subs, t) do
    case t do
      %Var{} = v -> Subst.insert(subs, v, Var.clone(v))
      [t | ts] -> subs |> build_s(t) |> build_s(ts)
      _ -> subs
    end
  end
end
