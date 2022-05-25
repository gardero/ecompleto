defmodule ECompleto.Rules.DERule do
  @moduledoc """
  Defines an Existential Rule with a body and a head.
  """
  defstruct body: [], head: [], clauses: [], type: :derule, alias: ""

  alias __MODULE__

  @type t() :: %DERule{
          body: [],
          head: [],
          clauses: [],
          type: :derule,
          alias: String.t()
        }

  defimpl String.Chars, for: DERule do
    def to_string(rule) do
      abody =
        rule.body
        |> Enum.map_join(", ", &(&1 |> String.Chars.to_string()))

      ahead =
        rule.head
        |> Enum.map_join(", ", fn hi ->
          thi = hi |> Enum.map_join(", ", fn l -> "#{l}" end)

          if hi |> length > 1 do
            "(#{thi})"
          else
            thi
          end
        end)
        |> Enum.join(", ")

      if rule.head |> length > 0 do
        "[#{ahead}] :- #{abody}."
      else
        "! :- #{abody}."
      end
    end
  end
end
