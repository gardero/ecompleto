defmodule ECompleto.Terms.Variable do
  @moduledoc """
  Defines a variable with possibly an index for standardization apart purposes.
  """
  @enforce_keys [:name]
  defstruct [:name, index: 0, type: :variable]
  alias __MODULE__

  @type t() :: %Variable{
          name: String.t(),
          index: non_neg_integer(),
          type: atom()
        }

  defimpl String.Chars, for: Variable do
    def to_string(v) do
      if v.index > 0 do
        "#{v.name}_#{v.index}"
      else
        v.name
      end
    end
  end
end
