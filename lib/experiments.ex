import ECompleto.Rewriting
import ExProf.Macro
require Logger

defmodule ECompleto.Experiments do
  @doc """
  Given a program and a query. The  UCQ rewriting of that query is found.
  """
  def rewrite(program_file, queries_file, index) do
    t0 = :os.system_time(:millisecond)
    p = ECompleto.Program.load_program(program_file)
    q = ECompleto.Program.load_program(queries_file).body
    p = ECompleto.Program.new_program(p.headers, [q |> Enum.fetch!(index) | p.body])

    p =
      if p.disj_rules |> length == 0 and (q |> Enum.fetch!(index)).type != :derule do
        %{p | :constraints => []}
      else
        p
      end

    res =
      p
      |> ECompleto.Rewriting.rewrite()
      |> ECompleto.Program.to_program(p.headers)

    Logger.info("It took #{:os.system_time(:millisecond) - t0} ms")
    res
  end

  @doc """
  Given a program and a query. The  UCQ rewriting of that query is found.
  """
  def erewrite(program_file, queries_file, index) do
    t0 = :os.system_time(:millisecond)
    p = ECompleto.Program.load_program(program_file)
    q = ECompleto.Program.load_program(queries_file).body
    p = ECompleto.Program.new_program(p.headers, [q |> Enum.fetch!(index) | p.body])

    p =
      if p.disj_rules |> length == 0 and (q |> Enum.fetch!(index)).type != :derule do
        %{p | :constraints => []}
      else
        p
      end

    res =
      p
      |> ECompleto.Rewriting.erewrite()
      |> ECompleto.Program.to_program(p.headers)

    Logger.info("It took #{:os.system_time(:millisecond) - t0} ms")
    res
  end

  def rewrite_plus_data(program_file, queries_file, index) do
    t0 = :os.system_time(:millisecond)
    p = ECompleto.Program.load_program(program_file)
    q = ECompleto.Program.load_program(queries_file).body
    p = ECompleto.Program.new_program(p.headers, [q |> Enum.fetch!(index) | p.body])

    p =
      if p.disj_rules |> length == 0 and (q |> Enum.fetch!(index)).type != :derule do
        %{p | :constraints => []}
      else
        p
      end

    res =
      p
      |> ECompleto.Rewriting.rewrite()
      |> ECompleto.Program.to_program(p.headers)

    Logger.info("It took #{:os.system_time(:millisecond) - t0} ms")
    ECompleto.Program.new_program(p.headers, p.facts ++ res.body)
  end

  def rewrite(program_file, queries_file) do
    t0 = :os.system_time(:millisecond)
    p = ECompleto.Program.load_program(program_file)
    q = ECompleto.Program.load_program(queries_file)
    p = ECompleto.Program.new_program(p.headers ++ q.headers, q.body ++ p.body)

    p =
      if p.disj_rules |> length == 0 and (q |> Enum.all?(fn qi -> qi.type != :derule end) ) do
        %{p | :constraints => []}
      else
        p
      end

    res =
      p
      |> ECompleto.Rewriting.rewrite()
      |> ECompleto.Program.to_program(p.headers)

    Logger.info("It took #{:os.system_time(:millisecond) - t0} ms")
    res
  end

  @doc """
  Given a program with facts. A query is answered.
  """
  def answer(program_file, queries_file, index) do
    cert =
      rewrite_plus_data(program_file, queries_file, index)
      |> ECompleto.Facts.FactsDB.answer()
      |> Stream.map(fn tuple ->
        ECompleto.Clauses.new_clause([ECompleto.Queries.new_answer_atom(tuple)], [])
      end)
      |> ECompleto.Program.to_program()

    cert
  end

  # def rewrite(program_file, queries_file) do
  #   t0 = :os.system_time(:millisecond)
  #   p = ECompleto.Program.load_program(program_file)
  #   q = ECompleto.Program.load_program(queries_file)
  #   p = %{ p | :disj_rules => (q.disj_rules++p.disj_rules)}
  #   p = %{ p | :queries => (q.queries++ p.queries)}
  #   p = if (q.disj_rules |> length == 0) do %{ p | :constraints => []} else p end
  #   res = p |> ECompleto.Rewriting.rewrite
  #   Logger.info("It took #{:os.system_time(:millisecond)-t0} ms")
  #   res
  # end

  def rewrite(program_file) do
    p = ECompleto.Program.load_program(program_file)
    p |> ECompleto.Rewriting.rewrite()
  end

  def to_file(lines, file_name) do
    lines = lines |> Enum.map(fn l -> "#{l}" end) |> Enum.join("\n")
    File.write(file_name, lines)
  end

  def compare_queries(file1, file2) do
    # profile do
    p1 =
      (ECompleto.Program.load_program(file1)
       |> Map.get(:queries)
       |> Enum.flat_map(fn q -> q.clauses end)) ++
        (ECompleto.Program.load_program(file1)
         |> Map.get(:constraints)
         |> Enum.flat_map(fn q -> q.clauses end))

    p2 =
      (ECompleto.Program.load_program(file2)
       |> Map.get(:queries)
       |> Enum.flat_map(fn q -> q.clauses end)) ++
        (ECompleto.Program.load_program(file2)
         |> Map.get(:constraints)
         |> Enum.flat_map(fn q -> q.clauses end))

    IO.inspect("comp1")

    {_, [], []} =
      p1
      |> most_general_covers(p2)

    IO.inspect("comp2")

    {_, [], []} =
      p2
      |> most_general_covers(p1)

    # end
    true
  end
end

## ECompleto.Experiments.rewrite('ontologies/travel.dlgp', 'ontologies/travel.queries.txt', 0) |> Enum.map(&("#{&1}"))
## ECompleto.Experiments.rewrite('ontologies/travel.dlgp', 'ontologies/travel.queries.txt', 0) |> ECompleto.Program.to_program |> ECompleto.Program.to_file('ontologies/travel.rewriting.dlgp')
