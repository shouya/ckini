defmodule Ckini.Context do
  @moduledoc """
  An intermediate state of computation.

  It contains a substitution of variables and a set of different types
  of constraints:

  - neq: created using `neq` goal
  - sym: created using `symbolo` goal
  - abs: created using `absento` goal
  """

  alias Ckini.{Subst, Term, Var}

  require Term
  require Subst

  defstruct subst: Subst.new(), neq: [], sym: MapSet.new(), abs: MapSet.new()

  @type t :: %__MODULE__{
          subst: Subst.t(),
          neq: [Subst.t()],
          sym: MapSet.new(Var.t()),
          abs: MapSet.new({Term.t(), Term.t()})
        }

  def new() do
    %__MODULE__{}
  end

  @spec unify(t(), Term.t(), Term.t()) :: nil | t()
  def unify(c, v, w) do
    case Subst.unify(c.subst, v, w) do
      nil -> nil
      s when Subst.is_empty(s) -> c
      extra_s -> verify(%{c | subst: Subst.concat(extra_s, c.subst)})
    end
  end

  @spec disunify(t(), Term.t(), Term.t()) :: nil | t()
  def disunify(c, v, w) do
    case Subst.unify(c.subst, v, w) do
      nil -> c
      s when Subst.is_empty(s) -> nil
      extra_s -> %{c | neq: [extra_s | c.neq]}
    end
  end

  @spec verify(t()) :: t() | nil
  def verify(c) do
    c
    |> verify_neq()
    |> verify_sym()
    |> verify_abs()
  end

  @spec verify_neq(t() | nil) :: t() | nil
  def verify_neq(nil), do: nil
  def verify_neq(%{neq: [], subst: _} = ctx), do: ctx

  def verify_neq(%{neq: [c | cs], subst: sub} = ctx) do
    case Subst.verify_neq(sub, c) do
      nil ->
        nil

      new_c ->
        case verify_neq(%{ctx | neq: cs}) do
          nil -> nil
          new_ctx -> %{new_ctx | neq: [new_c | new_ctx.neq]}
        end
    end
  end

  @spec verify_sym(t() | nil) :: t() | nil
  def verify_sym(nil), do: nil
  def verify_sym(%{sym: []} = ctx), do: ctx

  def verify_sym(%{subst: sub, sym: syms} = ctx) do
    not_sym? = fn var ->
      t = Subst.walk(sub, var)
      not (Term.is_symbol(t) || Term.var?(t))
    end

    case Enum.find(syms, not_sym?) do
      nil -> ctx
      _ -> nil
    end
  end

  @spec verify_abs(t() | nil) :: t() | nil
  def verify_abs(nil), do: nil

  def verify_abs(%{abs: vs, subst: subst} = ctx) do
    case Enum.find(vs, fn {t, u} -> Term.contains?(t, u, subst) end) do
      nil -> ctx
      _ -> nil
    end
  end

  @doc """
  Retain only the constraints related to the given term.
  """
  @spec purify(t(), Subst.t()) :: t()
  def purify(ctx, r) do
    %{
      ctx
      | neq: purify_neq(ctx.neq, r, ctx.subst),
        sym: purify_sym(ctx.sym, r),
        abs: purify_abs(ctx.abs, r, ctx.subst)
    }
  end

  @spec purify_neq([Subst.t()], Subst.t(), Subst.t()) :: [Subst.t()]
  def purify_neq(cs, r, subst) do
    cs
    |> Enum.map(fn c -> Subst.deep_walk(subst, Subst.to_pairs(c)) end)
    |> Enum.reject(&Enum.empty?/1)
    |> Enum.reject(fn c -> Subst.anyvar?(r, c) end)
    |> Enum.map(fn c -> Subst.deep_walk(r, c) end)
  end

  def purify_sym(vs, r) do
    vs
    |> Enum.reject(fn v -> Term.var?(Subst.walk(r, v)) end)
    |> Enum.map(fn v -> Subst.walk(r, v) end)
  end

  def purify_abs(abs, r, subst) do
    abs
    |> Enum.map(fn {t, u} -> Subst.deep_walk(subst, {t, u}) end)
    |> Enum.reject(fn {t, _u} -> Term.concrete?(t) end)
    |> Enum.reject(fn {t, u} -> Subst.anyvar?(r, t) or Subst.anyvar?(r, u) end)
    |> Enum.map(fn {t, u} -> Subst.deep_walk(r, {t, u}) end)
  end

  @spec add_sym_constraint(t, Var.t(), Term.t()) :: t() | nil
  def add_sym_constraint(%{sym: sym} = ctx, v, t) do
    cond do
      Term.is_symbol(t) -> %{ctx | sym: MapSet.delete(sym, v)}
      Term.var?(t) -> %{ctx | sym: MapSet.put(sym, v)}
      true -> nil
    end
  end

  @spec add_abs_constraint(t, Term.t(), Term.t()) :: t() | nil
  def add_abs_constraint(%{subst: subst, abs: abs} = ctx, t, u) do
    cond do
      Term.concrete?(t, subst) -> ctx
      true -> %{ctx | abs: MapSet.put(abs, {t, u})}
    end
  end

  @spec constraints(t()) :: keyword()
  def constraints(ctx) do
    [
      neq: ctx.neq,
      sym: ctx.sym,
      abs: ctx.abs
    ]
    |> Enum.reject(fn {_, v} -> Enum.empty?(v) end)
  end
end
