defmodule Ckini do
  defmacro __using__(opts \\ []) do
    style =
      case Keyword.get(opts, :style, :macro) do
        :macro -> quote do: import(Ckini.Macro)
        :functional -> quote do: import(Ckini.Functional)
      end

    quote do
      unquote(style)
      import Ckini.Goals
      alias Ckini.Var
    end
  end
end
