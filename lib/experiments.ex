require Logger

defmodule ECompleto.Experiments do
  @moduledoc false

  alias ECompleto.{Clauses, Facts.FactsDB, Program, Queries, Rewriting}

  @doc """
  Given a program and a query. The  UCQ rewriting of that query is found.
  """
  @spec rewrite(String.t(), String.t(), non_neg_integer()) :: Program.t()
  def rewrite(program_file, queries_file, index) do
    t0 = :os.system_time(:millisecond)
    p = Program.load_program(program_file)
    q = Program.load_program(queries_file).body
    p = Program.new_program(p.headers, [q |> Enum.fetch!(index) | p.body])

    p =
      if p.disj_rules |> length == 0 and (q |> Enum.fetch!(index)).type != :derule do
        %{p | :constraints => []}
      else
        p
      end

    res =
      p
      |> Rewriting.rewrite()
      |> Program.to_program(p.headers)

    Logger.info("It took #{:os.system_time(:millisecond) - t0} ms")
    res
  end

  @doc """
  Given a program and a query. The  UCQ rewriting of that query is found using only the existential rules.
  """
  @spec erewrite(String.t(), String.t(), non_neg_integer()) :: Program.t()
  def erewrite(program_file, queries_file, index) do
    t0 = :os.system_time(:millisecond)
    p = Program.load_program(program_file)
    q = Program.load_program(queries_file).body
    p = Program.new_program(p.headers, [q |> Enum.fetch!(index) | p.body])

    p =
      if p.disj_rules |> length == 0 and (q |> Enum.fetch!(index)).type != :derule do
        %{p | :constraints => []}
      else
        p
      end

    res =
      p
      |> Rewriting.erewrite()
      |> Program.to_program(p.headers)

    Logger.info("It took #{:os.system_time(:millisecond) - t0} ms")
    res
  end

  @spec rewrite_plus_data(String.t(), String.t(), non_neg_integer()) :: Program.t()
  def rewrite_plus_data(program_file, queries_file, index) do
    t0 = :os.system_time(:millisecond)
    p = Program.load_program(program_file)
    q = Program.load_program(queries_file).body
    p = Program.new_program(p.headers, [q |> Enum.fetch!(index) | p.body])

    p =
      if p.disj_rules |> length == 0 and (q |> Enum.fetch!(index)).type != :derule do
        %{p | :constraints => []}
      else
        p
      end

    res =
      p
      |> Rewriting.rewrite()
      |> Program.to_program(p.headers)

    Logger.info("It took #{:os.system_time(:millisecond) - t0} ms")
    Program.new_program(p.headers, p.facts ++ res.body)
  end

  @spec rewrite(String.t(), String.t()) :: Program.t()
  def rewrite(program_file, queries_file) do
    t0 = :os.system_time(:millisecond)
    p = Program.load_program(program_file)
    q = Program.load_program(queries_file)
    p = Program.new_program(p.headers ++ q.headers, q.body ++ p.body)

    p =
      if p.disj_rules |> length == 0 and q |> Enum.all?(fn qi -> qi.type != :derule end) do
        %{p | :constraints => []}
      else
        p
      end

    res =
      p
      |> Rewriting.rewrite()
      |> Program.to_program(p.headers)

    Logger.info("It took #{:os.system_time(:millisecond) - t0} ms")
    res
  end

  @doc """
  Given a program with facts. A query is answered.
  """
  @spec answer(String.t(), String.t(), non_neg_integer()) :: Program.t()
  def answer(program_file, queries_file, index) do
    cert =
      rewrite_plus_data(program_file, queries_file, index)
      |> FactsDB.answer()
      |> Stream.map(fn tuple ->
        Clauses.new_clause([Queries.new_answer_atom(tuple)], [])
      end)
      |> Program.to_program()

    cert
  end

  # def rewrite(program_file, queries_file) do
  #   t0 = :os.system_time(:millisecond)
  #   p = load_program(program_file)
  #   q = load_program(queries_file)
  #   p = %{ p | :disj_rules => (q.disj_rules++p.disj_rules)}
  #   p = %{ p | :queries => (q.queries++ p.queries)}
  #   p = if (q.disj_rules |> length == 0) do %{ p | :constraints => []} else p end
  #   res = p |> rewrite
  #   Logger.info("It took #{:os.system_time(:millisecond)-t0} ms")
  #   res
  # end
  @doc """
  rewrites the queries of an ontology using the rules.
  """
  @spec rewrite(String.t()) :: [...]
  def rewrite(program_file) do
    p = Program.load_program(program_file)

    p
    |> Rewriting.rewrite()
    |> Program.to_program(p.headers)
  end

  @doc """
  rewrites the queries of an ontology using only the existential rules.
  """
  @spec erewrite(String.t()) :: [...]
  def erewrite(program_file) do
    p = Program.load_program(program_file)

    p
    |> Rewriting.erewrite()
    |> Program.to_program(p.headers)
  end

  def to_file(lines, file_name) do
    lines = lines |> Enum.map_join("\n", fn l -> "#{l}" end)
    File.write(file_name, lines)
  end

  def compare_queries(file1, file2) do
    # profile do
    p1 =
      (Program.load_program(file1)
       |> Map.get(:queries)
       |> Enum.flat_map(fn q -> q.clauses end)) ++
        (Program.load_program(file1)
         |> Map.get(:constraints)
         |> Enum.flat_map(fn q -> q.clauses end))

    p2 =
      (Program.load_program(file2)
       |> Map.get(:queries)
       |> Enum.flat_map(fn q -> q.clauses end)) ++
        (Program.load_program(file2)
         |> Map.get(:constraints)
         |> Enum.flat_map(fn q -> q.clauses end))

    #    IO.inspect("comp1")

    {ucq1, add1, rem1} =
      p1
      |> Rewriting.most_general_covers(p2)

    Logger.debug("COVER1 #{ucq1 |> length}")
    Logger.debug("Add1 #{add1 |> length} i.e. ")

    if add1 |> length > 0 do
      #      Logger.debug("New CQs #{added |> Enum.map_join(", ", &"#{&1}")}")
      add1
      |> Enum.each(fn c ->
        Logger.debug("adding CQ #{c}")
      end)
    end

    Logger.debug("Rem1 #{rem1 |> Enum.map_join(", ", &"#{&1}")}")
    #    IO.inspect("comp2")

    {ucq2, add2, rem2} =
      p2
      |> Rewriting.most_general_covers(p1)

    Logger.debug("COVER2 #{ucq2 |> length}")
    Logger.debug("Add2 #{add2 |> length} i.e.")

    if add2 |> length > 0 do
      #      Logger.debug("New CQs #{added |> Enum.map_join(", ", &"#{&1}")}")
      add2
      |> Enum.each(fn c ->
        Logger.debug("adding CQ #{c}")
      end)
    end

    Logger.debug("Rem2 #{rem2 |> Enum.map_join(", ", &"#{&1}")}")
    # end
    add1 == [] and rem1 == [] and add2 == [] and rem2 == []
  end

  @doc """
  compares the clauses of existential rules for subsumption.
  """

  def compare_rules(file1, file2) do
    # profile do
    p1 =
      (Program.load_program(file1)
       |> Map.get(:rules)
       |> Enum.flat_map(fn q -> q.clauses end))

    p2 =
      (Program.load_program(file2)
       |> Map.get(:rules)
       |> Enum.flat_map(fn q -> q.clauses end))

    #    IO.inspect("comp1")

    {ucq1, add1, rem1} =
      p1
      |> Rewriting.most_general_covers(p2)

    Logger.debug("COVER1 #{ucq1 |> length}")
    Logger.debug("Add1 #{add1 |> length} i.e. ")

    if add1 |> length > 0 do
      #      Logger.debug("New CQs #{added |> Enum.map_join(", ", &"#{&1}")}")
      add1
      |> Enum.each(fn c ->
        Logger.debug("adding CQ #{c}")
      end)
    end

    Logger.debug("Rem1 #{rem1 |> Enum.map_join(", ", &"#{&1}")}")
    #    IO.inspect("comp2")

    {ucq2, add2, rem2} =
      p2
      |> Rewriting.most_general_covers(p1)

    Logger.debug("COVER2 #{ucq2 |> length}")
    Logger.debug("Add2 #{add2 |> length} i.e.")

    if add2 |> length > 0 do
      #      Logger.debug("New CQs #{added |> Enum.map_join(", ", &"#{&1}")}")
      add2
      |> Enum.each(fn c ->
        Logger.debug("adding CQ #{c}")
      end)
    end

    Logger.debug("Rem2 #{rem2 |> Enum.map_join(", ", &"#{&1}")}")
    # end
    add1 == [] and rem1 == [] and add2 == [] and rem2 == []
  end

  @doc """
  computes the cover of the queries and constraints in an ontology.
  """
  def cover(file1) do
    # profile do
    p = Program.load_program(file1)

    p1 =
      (p
       |> Map.get(:queries)
       |> Enum.flat_map(fn q -> q.clauses end)) ++
        (p
         |> Map.get(:constraints)
         |> Enum.flat_map(fn q -> q.clauses end))

    Logger.info("loaded...")

    p1
    |> Rewriting.most_general([])
  end
end

## ECompleto.Experiments.rewrite('ontologies/travel.dlgp', 'ontologies/travel.queries.txt', 0) |> Enum.map(&("#{&1}"))
## ECompleto.Experiments.rewrite('ontologies/travel.dlgp', 'ontologies/travel.queries.txt', 0) |> to_program |> to_file('ontologies/travel.rewriting.dlgp')
