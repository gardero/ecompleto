import ECompleto.Rewriting
import ExProf.Macro
require Logger

defmodule ECompleto.Experiments do
  def rewrite(program_file, queries_file, index) do
    p = ECompleto.Program.load_program(program_file)
    q = ECompleto.Program.load_program(queries_file).disj_rules
    p = %{ p | :disj_rules => [q |> Enum.fetch!(index) | p.disj_rules]}
    p |> ECompleto.Rewriting.rewrite
  end

  def rewrite(program_file, queries_file) do
    t0 = :os.system_time(:millisecond)
    p = ECompleto.Program.load_program(program_file)
    q = ECompleto.Program.load_program(queries_file)
    p = %{ p | :disj_rules => (q.disj_rules++p.disj_rules)}
    p = %{ p | :queries => (q.queries++ p.queries)}
    p = if (q.disj_rules |> length == 0) do %{ p | :constraints => []} else p end
    res = p |> ECompleto.Rewriting.rewrite
    Logger.info("It took #{:os.system_time(:millisecond)-t0} ms")
    res
  end

  def rewrite(program_file) do
    p = ECompleto.Program.load_program(program_file)
    p |> ECompleto.Rewriting.rewrite
  end

  def to_file(lines, file_name) do
    lines = lines |> Enum.map(fn l -> "#{l}" end) |> Enum.join("\n")
    File.write(file_name, lines)
  end

  def compare_queries(file1, file2) do
    # profile do
      p1 = (ECompleto.Program.load_program(file1) |> Map.get(:queries) |> Enum.flat_map(fn q-> q.clauses end)) ++ (ECompleto.Program.load_program(file1) |> Map.get(:constraints) |> Enum.flat_map(fn q-> q.clauses end))
      p2 =( ECompleto.Program.load_program(file2) |> Map.get(:queries) |> Enum.flat_map(fn q-> q.clauses end) )  ++ (ECompleto.Program.load_program(file2) |> Map.get(:constraints) |> Enum.flat_map(fn q-> q.clauses end))
      IO.inspect("comp1")
      {_, [], []} = p1
        |> most_general_covers(p2)
      IO.inspect("comp2")
      {_, [], []} = p2
      |> most_general_covers(p1)
    # end
    true
  end


end


## ECompleto.Experiments.rewrite('ontologies/travel.dlgp', 'ontologies/travel.queries.txt', 0) |> Enum.map(&("#{&1}"))
## ECompleto.Experiments.rewrite('ontologies/travel.dlgp', 'ontologies/travel.queries.txt', 0) |> ECompleto.Program.to_program |> ECompleto.Program.to_file('ontologies/travel.rewriting.dlgp')
