defmodule ECompleto.Facts.FactsDB do
  @moduledoc false
  import ECompleto.Clauses
  import ECompleto.Unification.Substitutions
  import ECompleto.Unification

  alias ECompleto.Clauses.Literal
  alias ECompleto.Program
  alias ECompleto.Rules.ERule
  alias ECompleto.Queries.CQuery

  def test_answer() do
    Program.load_program('examples/facts_queries.dlpg')
    |> answer
    |> Enum.map(fn x -> x end)
  end

  @doc """
  takes a programs and answers all the queries with respect to the facts.
  """
  @spec answer(Program.t()) :: Enumerable.t()
  def answer(program = %Program{}) do
    db_name = :temporal_db
    delete_facts_db(db_name)
    init_facts_db(db_name)

    program.facts
    |> Enum.each(fn r ->
      %{head: h} = r
      h |> Enum.map(fn a -> add_atom(a, db_name) end)
    end)

    res =
      program.queries
      |> Stream.flat_map(fn q ->
        answer_cq(q, db_name)
      end)
      |> Stream.uniq()

    res

    #   {constraint_clauses, _, _} = (program.queries |> Enum.flat_map(&(&1.clauses))) ++ (program.constraints |> Enum.flat_map(&(&1.clauses)))
    #   |> most_general([])
    #   Logger.info("CQs #{constraint_clauses|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")
    #   Logger.info("Rules #{program.rules|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")
    #   Logger.info("DRules #{program.disj_rules|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")

    # rewrite_disj_queue(constraint_clauses, constraint_clauses, [], program.rules, program.disj_rules)
  end

  @doc """
  creates an ETS table to put facts inside.
  """
  @spec init_facts_db(atom()) :: any()
  def init_facts_db(db_name) do
    :ets.new(db_name, [:bag, :protected, :named_table])
  end

  @doc """
  deletes a ETS table.
  """
  @spec delete_facts_db(atom()) :: any()
  def delete_facts_db(db_name) do
    if :ets.whereis(db_name) != :undefined do
      :ets.delete(db_name)
    end
  end

  @doc """
  adds an atom belonging to a fact into a facts DB.
  """
  @spec add_atom(Literal.t(), atom()) :: any()
  def add_atom(atom, db_name) do
    # IO.inspect "Adding #{atom}"
    %{predicate: f, arguments: args} = atom
    :ets.insert(db_name, {f, args, atom})
  end

  @doc """
  adds a fact to a facts DB
  """
  @spec add_fact(ERule.t(), atom()) :: any()
  def add_fact(fact = %ERule{}, db_name) do
    %{head: head} = fact

    # do some renaming first
    head
    |> Enum.each(fn l -> l |> add_atom(db_name) end)
  end

  @doc """
  creates a pattern to match the constants in the arguments of a query atom agains the existing atoms in the DB.
  """
  def arguments_pattern([]) do
    []
  end

  def arguments_pattern([x | l]) do
    lp = l |> arguments_pattern
    t = Map.get(x, :type)

    if t == :variable do
      [:_ | lp]
    else
      [x | lp]
    end
  end

  @doc """
  gets the atoms in the DB that unify with a certain query atom.
  """
  @spec unifying_atoms(Literal.t(), atom()) :: Enumerable.t()
  def unifying_atoms(atom, db_name) do
    %{predicate: f, arguments: args} = atom
    args_pattern = arguments_pattern(args)

    :ets.match_object(db_name, {f, args_pattern, :"$1"})
    |> Stream.map(fn {_key, _args, literal} -> literal end)
    |> unifying_single_literals_mgu_stream(atom)
  end

  @doc """
  unfies all the atoms in the query with atoms in the DB.
  returns a collection of the unifiers.
  """
  def query_subset_unify([], _db_name, u) do
    [u]
  end

  @spec query_subset_unify([Literal.t()], atom(), map()) :: Enumerable.t()
  def query_subset_unify([atom | list_atoms], db_name, u) do
    unifying_atoms(atom, db_name)
    |> Stream.flat_map(fn {_lit, mg_unifier} ->
      list_atoms
      |> apply_substitution(mg_unifier)
      |> query_subset_unify(db_name, compose(u, mg_unifier))
    end)
  end

  @spec query_subset_unify([Literal.t()], atom()) :: Enumerable.t()
  def query_subset_unify(list_atoms, db_name) do
    query_subset_unify(list_atoms, db_name, new_substitution())
  end

  @doc """
  finds all the answer to a query with respect to a DB.
  Returns a collection of answer tupples.
  """
  @spec answer_cq(CQuery.t(), atom()) :: Enumerable.t()
  def answer_cq(cquery, db_name) do
    %{body: b} = cquery

    b
    |> query_subset_unify(db_name)
    |> Stream.map(fn u ->
      #      IO.inspect(cquery.answer_tuple)
      #      IO.inspect(u)
      cquery.answer_tuple |> apply_substitution(u)
    end)
  end
end
