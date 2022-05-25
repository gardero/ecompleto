import ECompleto.Clauses

defmodule ECompleto.Queries.CQuery do
  @moduledoc """
  Defines an Existential Rule with a body and a head.
  """
  defstruct body: [], answer_tuple: [], clauses: [], type: :cquery, alias: ""

  defimpl String.Chars, for: ECompleto.Queries.CQuery do
    def to_string(rule) do
      abody =
        rule.body
        |> Enum.map_join(", ", &(&1 |> String.Chars.to_string()))

      ahead =
        rule.answer_tuple
        |> Enum.map_join(", ", &(&1 |> String.Chars.to_string()))

      if rule.answer_tuple |> length > 0 do
        "?(#{ahead}):-#{abody}."
      else
        "? :-#{abody}."
      end
    end
  end
end

defmodule ECompleto.Queries do
  @moduledoc false
  import ECompleto.Clauses
  import Logger

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

  def get_body(%ECompleto.Queries.CQuery{body: body}), do: body

  def get_body(query) do
    c_head =
      query.head
      |> Enum.map(fn atom ->
        atom |> Enum.at(0) |> complement
      end)

    query.body ++ c_head
  end

  def contains(query1, query2) do
    lines =
      query2
      |> get_body
      |> Enum.map(fn literal ->
        if literal |> is_positive? do
          # todo skollemize first.
          new_clause([literal |> freeze], [])
        else
          new_clause([], [literal |> freeze])
        end
      end)

    cnf = [query1 |> get_body |> Enum.map(&complement(&1)) |> new_clause_mix | lines]
    Logger.debug("CNF problem {#{cnf |> Enum.join(", ")}}")
    cnf |> refutations_unit
  end

  # def new_query(body) do
  #   new_query([], body)
  # end
end
