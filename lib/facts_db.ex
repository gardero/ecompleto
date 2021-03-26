
defmodule ECompleto.Facts.FactsDB do


  import ECompleto.Clauses
  import ECompleto.Unification.Substitutions
  import ECompleto.Unification

  def test_answer() do
    p = ECompleto.Program.load_program('examples/facts_queries.dlpg')
    p |> answer
      |> Enum.map(fn x-> x end)
  end

  def answer(program=%ECompleto.Program{}) do
    db_name = :temporal_db
    init_factsDB(db_name)
    program.facts
      |> Enum.map(fn r->
        %{head: h} = r
        h |> Enum.map(fn a-> add_atom(a,db_name) end)

    end)
    program.queries
      |>  Enum.map(fn q->
         answer_cq(q, db_name)
           |> Enum.map(fn u->
            IO.inspect(q.answer_tuple)
            IO.inspect(u)
             q.answer_tuple |> apply_substitution(u)

          end)
    end)




    #   {constraint_clauses, _, _} = (program.queries |> Enum.flat_map(&(&1.clauses))) ++ (program.constraints |> Enum.flat_map(&(&1.clauses)))
    #   |> most_general([])
    #   Logger.info("CQs #{constraint_clauses|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")
    #   Logger.info("Rules #{program.rules|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")
    #   Logger.info("DRules #{program.disj_rules|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")

    # rewrite_disj_queue(constraint_clauses, constraint_clauses, [], program.rules, program.disj_rules)
  end

  def init_factsDB(db_name) do
    :ets.new(db_name, [:bag, :protected, :named_table])
  end


  def add_atom(atom, db_name) do
    %{predicate: f, arguments: args} = atom
    :ets.insert(db_name, {f, args, atom})
  end

  def arguments_pattern([]) do
    []
  end
  def arguments_pattern([x|l]) do
    lp =  l |> arguments_pattern
    t = Map.get(x, :type)
    if t== :variable do
      [:_|lp]
    else
      [x| lp]
    end
  end

  def unifying_atoms(atom, db_name) do
    %{predicate: f, arguments: args} = atom
    args_pattern = arguments_pattern(args)
    :ets.match_object(db_name, {f, args_pattern, :"$1"})
      |> Stream.map(fn {_key,_args, literal} -> literal end)
      |> unifying_single_literals_mgu_stream(atom)
  end


  def query_subset_unify([], _db_name, u) do
    [u]
  end

  def query_subset_unify([atom | list_atoms], db_name, u) do
    unifying_atoms(atom, db_name)
      |> Enum.flat_map(fn {_lit, mg_unifier} ->
        list_atoms
          |> apply_substitution(mg_unifier)
          |> query_subset_unify(db_name, compose(u, mg_unifier))
      end)
  end

  def query_subset_unify(list_atoms, db_name) do
    query_subset_unify(list_atoms, db_name, new_substitution())
  end

  def answer_cq(cquery, db_name) do
    %{body: b} = cquery
    b |> query_subset_unify(db_name)
  end

  def add_fact(fact, db_name) do

    %{head: head} = fact

    # do some renaming first

    head |>
      add_atom(db_name)

  end

end
