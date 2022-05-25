defmodule ECompleto.Terms.FTerm do
  @moduledoc """
  Defines a functional term with a functor and a list of arguments.
  """
  @enforce_keys [:functor]
  defstruct [:functor, arguments: [], skolem: false, type: :term]

  alias __MODULE__
  alias ECompleto.Terms

  @type t() :: %FTerm{
          functor: String.t() | atom(),
          arguments: Terms.terms_list(),
          skolem: boolean(),
          type: atom()
        }

  defimpl String.Chars, for: FTerm do
    def to_string(term) do
      args =
        term.arguments
        |> Enum.map_join(", ", &(&1 |> String.Chars.to_string()))

      f =
        if term.skolem do
          "#{term.functor |> String.Chars.to_string()}'"
        else
          term.functor |> String.Chars.to_string()
          pred = "#{term.functor}"

          if pred |> String.starts_with?("http://") do
            "<#{pred}>"
          else
            pred
          end
        end

      if term.arguments |> length > 0 do
        "#{f}(#{args})"
      else
        f
      end
    end
  end
end
