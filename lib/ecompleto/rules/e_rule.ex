defmodule ECompleto.Rules.ERule do
  @moduledoc """
  Defines an Existential Rule with a body and a head.
  """
  defstruct body: [], head: [], clauses: [], type: :erule, alias: ""
  alias __MODULE__
  alias ECompleto.Clauses.Atom

  @type t() :: %ERule{
          body: Atom.literals(),
          head: Atom.literals(),
          clauses: [] | [ECompleto.Clauses.Clause.t()],
          type: :erule,
          alias: String.t()
        }

  defimpl String.Chars, for: ERule do
    def to_string(rule) do
      abody =
        rule.body
        |> Enum.map_join(", ", &(&1 |> String.Chars.to_string()))

      ahead =
        rule.head
        |> Enum.map_join(", ", &(&1 |> String.Chars.to_string()))

      if rule.head |> length > 0 do
        if rule.body |> length > 0 do
          "#{ahead} :- #{abody}."
        else
          "#{ahead}."
        end
      else
        "! :- #{abody}."
      end
    end
  end
end
