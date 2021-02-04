defmodule ExCkini.Var do
  @moduledoc false

  defstruct [:sym, :original_sym]
  @type t :: %__MODULE__{}
end
