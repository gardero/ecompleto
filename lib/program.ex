


defmodule ECompleto.Program do

  defstruct [headers: [], body: [], rules: [], disj_rules: [], queries: [], constraints: [], facts: []]

  def new_program(headers, body) do
    %ECompleto.Program{
      headers: headers,
      body: body,
      rules: body |> Enum.filter(fn e ->
        e.type == :erule and e.body |> length >0 and e.head |> length >0
      end),
      disj_rules: body |> Enum.filter(fn e ->
        e.type == :derule and e.head |> length >0
      end),
      queries: body |> Enum.filter(fn e ->
        e.type == :cquery
      end),
      constraints: body |> Enum.filter(fn e ->
        e.type == :erule and e.body |> length >0 and e.head |> length == 0
      end),
      facts: body |> Enum.filter(fn e ->
        e.type == :erule and e.body |> length == 0
      end)
    }
  end


  import ECompleto.Utils

  defimpl String.Chars, for: ECompleto.Program do
    def to_string(prog) do
      "@rules\n #{(prog.rules++prog.disj_rules) |> to_string_list("\n")}\n@queries\n#{prog.queries |> to_string_list("\n")}\n@constraints\n#{prog.constraints |> to_string_list("\n")}\n@facts\n#{prog.facts |> to_string_list("\n")}"
    end
  end

  import ECompleto.Rules
  import ECompleto.Queries
  import ECompleto.Clauses

  def apply_prefix({prefix, rest}, maps) do
    Map.get(maps, prefix) <> rest
  end

  def apply_prefix(elem, _) do
    elem
  end

  def replace_prefixes(prog) do
      maps = prog.headers
      prog |> ECompleto.Unification.Transform.transform_terms(fn term ->
        case term do
          %{} ->
            type = Map.get(term, :type)
            case type do
              :term -> %{term | functor: term.functor |> apply_prefix(maps)}
              _ -> term
            end
          _ -> apply_prefix(term, maps)

        end
      end)
  end

  def load_program(file_name) do
    {:ok, text} = File.read(file_name)
    {_, tokens, _} = :dlgp_lexer.string(String.to_charlist(text))
    {:ok, prog} = tokens |> :dlgp_parser.parse
    prog |> replace_prefixes
  end

  def to_program(clauses) do
    body = clauses
      |> Enum.map(
        fn clause ->
          pch = clause.positive |> length()
          cond do
            pch == 0 -> if contains_answer_atom(clause) do new_cquery(clause) else new_erule([],clause.negative |> Enum.map(fn l -> l |> complement end)) end
            pch == 1 -> new_erule(clause.positive, clause.negative)
            pch > 1 -> new_erule(clause.positive |> Enum.map(fn l -> [l] end), clause.negative)
          end
        end
      )
    new_program([], body)
  end

  def to_file(prog, file_name) do
    File.write(file_name, "#{prog}")
  end

end




# ECompleto.Program.load_program('examples/mine.dlgp') |> ECompleto.Rewriting.rewrite |> Enum.map(&("#{&1}"))

###
##
##  p = ECompleto.Program.load_program('ontologies/travel.dlgp')
##  q = ECompleto.Program.load_program('ontologies/travel.queries.txt').disj_rules
##  p = %{ p | :disj_rules => [q |> Enum.fetch(0) |p.disj_rules]}
##  p |> ECompleto.Rewriting.rewrite
