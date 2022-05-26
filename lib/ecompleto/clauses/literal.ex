defmodule ECompleto.Clauses.Literal do
  @moduledoc """
  Defines an atom (positive or negated) with a predicate and a list of arguments.
  """
  @enforce_keys [:predicate]
  defstruct [:predicate, arguments: [], negated: false, key: {}, type: :atom]
  alias __MODULE__
  alias ECompleto.Terms

  @type t() :: %Literal{
          predicate: String.t() | atom(),
          arguments: Terms.terms_list(),
          negated: boolean(),
          key: {String.t(), integer()},
          type: :atom
        }

  @type literals() :: [Literal.t()] | Enumerable.t()

  defimpl String.Chars, for: Literal do
    def to_string(atom) do
      args =
        atom.arguments
        |> Enum.map(&(&1 |> String.Chars.to_string()))

      sign =
        if atom.negated do
          "-"
        else
          ""
        end

      pred = "#{atom.predicate}"

      f =
        if pred |> String.starts_with?("http://") or pred |> String.starts_with?("file://") do
          "#{sign}<#{pred}>"
        else
          "#{sign}#{pred}"
        end

      cond do
        pred == "=" -> "#{args |> Enum.join(" = ")}"
        atom.arguments |> length > 0 -> "#{f}(#{args |> Enum.join(", ")})"
        atom.arguments |> length == 0 -> f
      end
    end
  end
end
