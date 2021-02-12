defmodule Ckini.Context do
  @moduledoc false

  alias Ckini.{Subst, Term}

  defstruct subst: [], constraints: []

  @type t :: %__MODULE__{
          subst: Subst.t(),
          constraints: [Subst.t()]
        }

  def new(subst \\ [], constraints \\ []) do
    %__MODULE__{subst: subst, constraints: constraints}
  end

  @spec unify(t(), Term.t(), Term.t()) :: nil | t()
  def unify(c, v, w) do
    case Subst.unify(c.subst, v, w) do
      nil ->
        nil

      [] ->
        verify(c)

      extra_s ->
        verify(%{c | subst: Subst.concat(extra_s, c.subst)})
    end
  end

  def disunify(c, v, w) do
    case Subst.unify(c.subst, v, w) do
      nil -> c
      [] -> nil
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
end
