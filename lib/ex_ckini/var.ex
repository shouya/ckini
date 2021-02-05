defmodule ExCkini.Var do
  @moduledoc "A logic variable"

  # the original_sym stores the symbol used in the code. It is only used for
  defstruct [:sym, :original_sym]
  @type t :: %__MODULE__{}

  def new(sym) do
    %__MODULE__{
      sym: System.unique_integer([:positive, :monotonic]),
      original_sym: sym
    }
  end

  def eq?(%__MODULE__{sym: v1}, %__MODULE__{sym: v2}) do
    v1 == v2
  end
end
