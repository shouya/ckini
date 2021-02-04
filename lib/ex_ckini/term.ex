defmodule ExCkini.Term do
  @moduledoc false

  alias ExCkini.Var

  @type t ::
          atom()
          | binary()
          | integer()
          | Var.t()
          | [t()]

  def var?(%Var{}), do: true
  def var?(_), do: false
end
