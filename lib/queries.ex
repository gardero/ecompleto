import ECompleto.Clauses

defmodule ECompleto.Queries.CQuery do
  @doc """
  Defines an Existential Rule with a body and a head.
  """
  defstruct body: [], answer_tuple: [], clauses: [], type: :cquery, alias: ""

  defimpl String.Chars, for: ECompleto.Queries.CQuery do
    def to_string(rule) do
      abody =
        rule.body
        |> Enum.map(&(&1 |> String.Chars.to_string()))
        |> Enum.join(", ")

      ahead =
        rule.answer_tuple
        |> Enum.map(&(&1 |> String.Chars.to_string()))
        |> Enum.join(", ")

      if rule.answer_tuple |> length > 0 do
        "?(#{ahead}):-#{abody}."
      else
        "? :-#{abody}."
      end
    end
  end
end

defmodule ECompleto.Queries do
  def new_answer_atom(answer_tuple) do
    new_literal(:answer_atom, answer_tuple)
  end

  def is_answer_atom?(atom) do
    Map.get(atom, :predicate) == :answer_atom
  end

  # def new_query(answer_tuple, qbody) do
  #   c = qbody |> Enum.map(&(ECompleto.Terms.complement(&1)))
  #   %{
  #     answer_tuple: answer_tuple,
  #     body: qbody,
  #     clauses: [ECompleto.Clauses.new_clause([ECompleto.Clauses.complement(new_answer_atom(answer_tuple)) | c])],
  #     type: :query
  #   }
  # end

  def new_cquery(answer_tuple, qbody) do
    c = qbody |> Enum.map(&complement(&1))

    %ECompleto.Queries.CQuery{
      body: qbody,
      answer_tuple: answer_tuple,
      clauses: [new_clause([], [complement(new_answer_atom(answer_tuple)) | c])]
    }
  end

  def new_cquery(clause) do
    literals =
      clause.negative
      |> Enum.group_by(fn l -> l |> is_answer_atom? end, fn l -> complement(l) end)

    new_cquery(
      Map.get(literals, true) |> Enum.flat_map(fn l -> Map.get(l, :arguments) end),
      Map.get(literals, false)
    )
  end

  @spec new_nquery(any, any) :: ECompleto.Rules.DERule.t()
  def new_nquery(answer_tuple, qbody) do
    h = qbody |> Enum.filter(&is_negative?(&1)) |> Enum.map(&[complement(&1)])
    b = qbody |> Enum.filter(&is_positive?(&1))
    ECompleto.Rules.new_derule(h, [new_answer_atom(answer_tuple) | b])
  end

  def contains_answer_atom(clause) do
    clause.negative
    |> Enum.any?(fn l ->
      l |> is_answer_atom?
    end)
  end

  # def new_query(body) do
  #   new_query([], body)
  # end
end
