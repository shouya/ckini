defmodule ExCkini.Var do
  @moduledoc "A logic variable"

  # The sym stores the symbol used in the code. The id is what really
  # identifies the variable.
  defstruct [:sym, :id]
  @type t :: %__MODULE__{}

  def new(sym) do
    %__MODULE__{
      id: System.unique_integer([:positive, :monotonic]),
      sym: sym
    }
  end

  def eq?(%__MODULE__{id: v1}, %__MODULE__{id: v2}) do
    v1 == v2
  end
end
