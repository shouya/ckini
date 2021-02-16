defmodule Ckini.Context do
  @moduledoc false

  alias Ckini.{Subst, Term, Var}

  require Term
  require Subst

  defstruct subst: Subst.new(), neq: [], sym: MapSet.new()

  @type t :: %__MODULE__{
          subst: Subst.t(),
          neq: [Subst.t()],
          sym: MapSet.new(Var.t())
        }

  def new(subst \\ Subst.new(), neq \\ [], sym \\ MapSet.new()) do
    %__MODULE__{subst: subst, neq: neq, sym: sym}
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
    |> verify_sym()
    |> verify_neq()
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

  @doc """
  Retain only the constraints related to the given term.
  """
  @spec purify(t(), Subst.t()) :: t()
  def purify(ctx, r) do
    %{
      ctx
      | neq: purify_neq(ctx.neq, r, ctx.subst),
        sym: purify_sym(ctx.sym, r)
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

  @spec add_symbol_constraint(t, Var.t(), Term.t()) :: t() | nil
  def add_symbol_constraint(%{sym: sym} = ctx, v, t) do
    cond do
      Term.is_symbol(t) -> %{ctx | sym: MapSet.delete(sym, v)}
      Term.var?(t) -> %{ctx | sym: MapSet.put(sym, v)}
      true -> nil
    end
  end

  @spec constraints(t()) :: keyword()
  def constraints(ctx) do
    [
      neq: ctx.neq,
      sym: ctx.sym
    ]
    |> Enum.reject(fn {_, v} -> Enum.empty?(v) end)
  end
end
