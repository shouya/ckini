defmodule Ckini.Var do
  @moduledoc "A logic variable"

  # The sym stores the symbol used in the code. The id is what really
  # identifies the variable.
  defstruct [:sym, :id]
  @type t :: %__MODULE__{}

  def new() do
    %__MODULE__{
      id: System.unique_integer([:positive, :monotonic]),
      sym: nil
    }
  end

  def new(sym) do
    %__MODULE__{
      id: System.unique_integer([:positive, :monotonic]),
      sym: sym
    }
  end

  def eq?(%__MODULE__{id: v1}, %__MODULE__{id: v2}) do
    v1 == v2
  end

  def clone(%__MODULE__{sym: sym}), do: new(sym)
end

defimpl Inspect, for: Ckini.Var do
  def inspect(%{sym: nil, id: id}, _opts) do
    "var_#{id}"
  end

  def inspect(%{sym: sym, id: id}, _opts) do
    "#{sym}<#{id}>"
  end
end
