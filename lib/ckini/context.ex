defmodule Ckini.Context do
  @moduledoc false

  alias Ckini.{Subst, Term}

  require Term
  require Subst

  defstruct subst: Subst.new(), constraints: []

  @type t :: %__MODULE__{
          subst: Subst.t(),
          constraints: [Subst.t()]
        }

  def new(subst \\ Subst.new(), constraints \\ []) do
    %__MODULE__{subst: subst, constraints: constraints}
  end

  @spec unify(t(), Term.t(), Term.t()) :: nil | t()
  def unify(c, v, w) do
    case Subst.unify(c.subst, v, w) do
      nil -> nil
      s when Subst.is_empty(s) -> c
      extra_s -> verify(%{c | subst: Subst.concat(extra_s, c.subst)})
    end
  end

  def disunify(c, v, w) do
    case Subst.unify(c.subst, v, w) do
      nil -> c
      s when Subst.is_empty(s) -> nil
      extra_s -> %{c | constraints: [extra_s | c.constraints]}
    end
  end

  def verify(%{constraints: [], subst: _} = ctx), do: ctx

  def verify(%{constraints: [c | cs], subst: sub} = ctx) do
    case Subst.verify(sub, c) do
      nil ->
        nil

      new_c ->
        case verify(%{ctx | constraints: cs}) do
          nil -> nil
          new_ctx -> %{new_ctx | constraints: [new_c | new_ctx.constraints]}
        end
    end
  end

  def purify(%{constraints: cs}, t) do
    vars = Term.all_vars(t)

    cs
    |> Enum.map(fn subst ->
      Subst.filter_vars(subst, Subst.relevant_vars(subst, vars))
    end)
    |> Enum.reject(&Subst.is_empty/1)
  end

  def reify_constraints(_subst, []), do: []

  def reify_constraints(subst, cs) do
    {:never, Subst.deep_walk(subst, Subst.contraint_repr(cs))}
  end
end
