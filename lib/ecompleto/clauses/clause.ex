defmodule ECompleto.Clauses.Clause do
  @moduledoc """
  Defines a clause. It contains a list of positive literals and a list of negative literals.
  It also contains some other information to speed up computations, e.g. a list of pairs predicates/arity in the clause,
  a frozen (with variables turned into constants) version of the lists of positive, negative literals.

  """
  defstruct positive: [],
            negative: [],
            positive_keys: MapSet.new([]),
            negative_keys: MapSet.new([]),
            positive_frozen: [],
            negative_frozen: [],
            type: :clause

  alias __MODULE__
  alias ECompleto.Terms
  alias ECompleto.Clauses.Atom

  @type t() :: %Clause{
          positive: Atom.literals(),
          negative: Atom.literals(),
          positive_keys: map(),
          negative_keys: map(),
          positive_frozen: Atom.literals(),
          negative_frozen: Atom.literals(),
          type: atom()
        }

  defimpl String.Chars, for: Clause do
    def to_string(clause) do
      args =
        (clause.positive ++ clause.negative)
        |> Enum.map_join(", ", &(&1 |> String.Chars.to_string()))

      "[#{args}]"
    end
  end
end
