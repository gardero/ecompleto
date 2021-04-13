

defmodule ECompleto.Clauses.Atom do
  @doc """
  Defines an atom (positive or negated) with a predicate and a list of arguments.
  """
  @enforce_keys [:predicate]
  defstruct [:predicate , arguments: [], negated: false, key: {}, type: :atom]

  defimpl String.Chars, for: ECompleto.Clauses.Atom do
    def to_string(atom) do
      args = atom.arguments
        |> Enum.map(&( &1 |> String.Chars.to_string ))
      sign = if atom.negated do "-" else "" end
      pred = "#{atom.predicate}"
      f = if ( pred  |> String.starts_with?("http://") or pred  |> String.starts_with?("file://")) do
        "#{sign}<#{pred}>"
      else
        "#{sign}#{pred}"
      end
      cond do
        pred == "="                    -> "#{args |> Enum.join(" = ")}"
        atom.arguments |> length > 0   -> "#{f}(#{args |> Enum.join(", ")})"
        atom.arguments |> length == 0  -> f
      end
    end
  end

end

defmodule ECompleto.Clauses.Clause do
  @doc """
  Defines a clause. It contains a list of positive literals and a list of negative literals.
  It also contains some other information to speed up computations, e.g. a list of pairs perdicates/arity in the clause,
  a frozen (with variables turned into constants) version of the lits of positive, negative literals.

  """
  defstruct [
    positive: [],
    negative: [],
    positive_keys: MapSet.new([]),
    negative_keys: MapSet.new([]),
    positive_frozen: [] ,
    negative_frozen: [],
    type: :clause
  ]


  defimpl String.Chars, for: ECompleto.Clauses.Clause do
    def to_string(clause) do
      args = clause.positive ++ clause.negative
        |> Enum.map(&( &1 |> String.Chars.to_string ))
        |> Enum.join(", ")
      "[#{args}]"
    end
  end



end

defmodule ECompleto.Clauses do



  import ECompleto.Unification
  import ECompleto.Unification.Substitutions
  import ECompleto.Terms



  # alias ECompleto.Unification.apply_substitution

  @doc """
  Creates a clause with a list of positive literals and a list of negative literals
  """
  def new_clause(pliterals, nliterals) do
    p = pliterals |> Enum.uniq
    n = nliterals |> Enum.uniq
    %ECompleto.Clauses.Clause{
      positive: p,
      positive_frozen: p |> freeze,
      positive_keys: p |> MapSet.new(fn a -> a.key end),
      negative: n,
      negative_frozen: n |> freeze,
      negative_keys: n |> MapSet.new(fn a -> a.key end)
    }
  end

  @doc """
  Creates a clause with a list of positive literals and a list of negative literals by splitting the given list.
  """
  def new_clause(literals) do
    new_clause(literals |> Enum.filter(&(is_positive?(&1))), literals |> Enum.filter(&(is_negative?(&1))))
  end

  @doc """
  Creates an atom (positive literal) with its predicate and arguments
  """
  def new_literal(predicate, arguments) do
    new_literal(predicate, arguments, false)
  end

  @doc """
  Creates an literal with its predicate, arguments and the sign (negated or not)
  """
  def new_literal(predicate, arguments, negated) do
    %ECompleto.Clauses.Atom{ predicate: predicate, arguments: arguments, negated: negated, key: {predicate, arguments|> length}}
  end


  @doc """
  Tells if a literal is negative.
  """
  def is_negative?(literal) do
    Map.get(literal, :negated)
  end

  @doc """
  Tells if a literal is positive.
  """
  def is_positive?(literal) do
    !is_negative?(literal)
  end

  def is_answer_literal(literal) do
    Map.get(literal, :predicate) == :answer_atom
  end

  @doc """
  Gets the complement of a literal.
  """
  def complement(f) do
    %{f | negated: !f.negated}
  end

  @doc """
  Turns the variables into constants.
  """
  def freeze(literals) do
    literals |> ECompleto.Unification.Transform.transform_terms(fn term ->
      case term do
        %{} ->
          type = Map.get(term, :type)
          if type == :variable do
            new_term(term.name,[])
          else
            term
          end
        _ -> term
        end
    end)
  end


  @doc """
  iterates through the subformulas, terms and subterms of a formula.
  """
  def iterate_subterms(term) when is_list(term) do
    term
    |> Stream.flat_map(
      fn x -> iterate_subterms(x) end
    )
    |> Stream.concat([])
  end

  def iterate_subterms(term = %{}) do
    type = Map.get(term, :type)
    # IO.inspect(type)
    term_keys = cond do
      type == :clause -> [:negative, :positive]
      type in [:atom, :term] -> [:arguments]
      type in [:erule, :drule] -> [:clauses, :head, :body]
      type == :cquery -> [:clauses, :answer_tuple, :body]
      true -> []
    end
    term_keys
    |> Stream.flat_map(
      fn x -> Map.get(term,x) |> iterate_subterms end
    )
    |> Stream.concat([term])
  end

  def iterate_subterms(term) do
    [term]
  end


  def rename_aux(maps, term, pos0) do
    maps
      |> Enum.reduce(
        term,
        fn {k, v}, acc ->
          {res1,_} = v
            |> Enum.reduce(
              {acc, Map.get(pos0, k, 0)},
              fn i, {acc1, pos} ->
                {
                  apply_substitution(acc1,
                  new_substitution([{new_var(k,i), new_var(k,pos)}])
                  ),
                  pos+1
                }
              end
            )
          res1
        end
      )
  end

  @doc """
  computes the maximum indexes that the variables have in a formula.
  """
  defp max_indexes(term, initial) do
    vars = term
    |> iterate_subterms
    |> Enum.filter(&( is_map(&1) and Map.get(&1, :type)== :variable ))
    |> Enum.uniq
    |> Enum.sort
    vars
      |> Enum.group_by(&(Map.get(&1,:name)), &(Map.get(&1,:index)))
      |> Map.new(fn {name, indexes} ->
        {
          name,
          indexes |> Enum.reduce(
            initial |> Map.get(name,0),
            fn x, acc -> max(x+1, acc) end
          )
        }
      end
      )  |> Map.merge(initial, fn _k, v1, _v2 ->
        v1
      end)
  end

  @doc """
  Renames the variables of a term with a consecutive index starting from a minimum value specified in fr0.
  """
  def rename(term, fr0) do
    vars = term
    |> iterate_subterms
    |> Enum.filter(&( is_map(&1) and Map.get(&1, :type)== :variable ))
    |> Enum.uniq
    |> Enum.sort
    ## Builds a map with the variable names and the indexes in the term
    maps = vars
      |> Enum.group_by(&(Map.get(&1,:name)), &(Map.get(&1,:index)))

    ## Builds a map with the variable names and maximum indexes in the resulting renamed term
    fr = vars
      |> Enum.frequencies_by(&(Map.get(&1,:name)))
      |> Map.new(
          fn {k, f} ->
            {k, f + Map.get(fr0,k,0)}
          end
        )
      |> Map.merge(fr0, fn _k, v1, _v2 ->
        v1
      end)

    {rename_aux(maps, term, fr0), fr}
  end

  @doc """
  Renames the variables of a term with a consecutive index starting from zero.
  """
  def rename(term) do
    rename(term, %{})
  end

  @doc """
  buils a renaming substitution for a term such that the indexes of the variables start from a specified minimum value.
  """
  def build_renaming(term, fr0) do
    vars = term
    |> iterate_subterms
    |> Enum.filter(&( is_map(&1) and Map.get(&1, :type)== :variable ))
    |> Enum.uniq
    |> Enum.sort
    maps = vars |> Enum.map(fn var ->
      {var |> String.Chars.to_string,
      ECompleto.Terms.new_var(var.name, (var.index + (fr0 |> Map.get(var.name)))) }
    end)
    Map.new(maps)
  end

  @doc """
  renames two formulas so that they dont share the same variables.
  """
  def rename_appart(term1, term2) do
    fr1 = term1 |> max_indexes(%{})
    fr1 = term2 |> max_indexes(fr1)
    renaming = term2 |> build_renaming(fr1)
    {term1, term2 |> apply_substitution(renaming)}
  end


  @doc """
  Returns a stream of all the subsets of the list of literals that unify with a specified literal.
  """
  defp unifying_literals_stream(literals_list, literal) do
    literals_list
      |> Enum.filter(&(
        &1
          |> unify(literal,%{})
          |> Kernel.elem(0)
        )
        )
      |> ECompleto.Utils.subsets_stream
  end

  @doc """
  Returns a stream of mgus of the literals that unify with a specified literal.
  """
  defp unifying_single_literals_stream(literals_list, literal) do
    literals_list
      |> Enum.map(fn l-> l |> unify(literal, %{}) end)
      |> Enum.filter(fn {uni, _mg} -> uni end)
      |> Enum.map(fn {_uni, mg}-> mg end)
  end


  @doc """
  Returns a stream of literals and mgu pairs that unify with a specified literal.
  """
  def unifying_single_literals_mgu_stream(literals_list, literal) do
    literals_list
      |> Stream.map(fn l-> l |> unify(literal, %{}) end)
      |> Stream.filter(fn {uni, _mg} -> uni end)
  end

  defp unifying_literals(literals_list, literal) do
    literals_list
      |> Enum.filter(&(
        &1
          |> unify(literal,%{})
          |> Kernel.elem(0)
        )
        )
  end

  @doc """
  unifies the answer literals in a list of literals.
  """
  def unify_answer_literals(literals_list) do
    al = literals_list
      |> Enum.filter(fn l -> l |> is_answer_literal end
        )
    {true, mg} = mgu(al)
    if al |> length > 1 do
      [(al |> List.first) | (literals_list -- al)] |> apply_substitution(mg)
    else
      literals_list
    end
  end


  @doc """
  gives a stream of all the factos of a list of literals.
  """
  defp literal_factors_stream([]), do: []

  defp literal_factors_stream([literal| rest_clause]) do
    rest_clause
      |> unifying_literals_stream(literal)
      |> Stream.map(fn some_literals -> {[literal| some_literals], mgu([literal| some_literals])} end)
      |> Stream.filter(fn {_some_literals, {res, _mg_unifier}} -> res end)
      |> Stream.map(fn {some_literals, {_res, mg_unifier}} -> {some_literals, mg_unifier} end)
      |> Stream.concat(rest_clause |> literal_factors_stream)
  end


  @doc """
  Skolemizes the existential variables of a rule by replacing them with unique (local to the rule)
  Skolem fuctions. As long as we do rewritings with respect to clauses that do not contain Skolem terms this
  should be fine. However, in Chase algorithms we need to ensure that the nulls created have a globally unique
  function symbol.
  """
  def skolemize(head, body) do
    existential_vars = head
      |> ECompleto.Clauses.iterate_subterms
      |> Enum.filter(&( is_map(&1) and Map.get(&1, :type)== :variable and !contains?(body, &1)))
      |> Enum.sort
      |> Enum.uniq

    body_vars = body
      |> ECompleto.Clauses.iterate_subterms
      |> Enum.filter(&( is_map(&1) and Map.get(&1, :type)== :variable))
      |> Enum.sort
      |> Enum.uniq


    head = existential_vars
      |> Enum.reduce(
        head,
        fn v, acc ->
          acc |>  apply_substitution(
            new_substitution([{v, new_skterm(v.name, body_vars)}])
          )
        end
      )
    head
  end


  @doc """
  checks if clause1 subsumes clause2, i.e., clause2 can be dediced from clause1.
  """
  def subsumes(clause1, clause2) do
    if clause1.positive_keys |> MapSet.subset?(clause2.positive_keys)
      and clause1.negative_keys |> MapSet.subset?(clause2.negative_keys) do
        subset_unify(clause1.positive, clause2.positive_frozen) and subset_unify(clause1.negative, clause2.negative_frozen)
      else
        false
      end
  end

  @doc """
  checks if all the literals in a list unify (in a compatible way) with all the literals in another list.
  """
  def subset_unify([], _list2) do
    true
  end

  def subset_unify([literal|rest1], list2) do
    list2
      |> unifying_single_literals_stream(literal)
      |> Stream.map(fn mg_unifier ->
        rest1 |> apply_substitution(mg_unifier)
      end)
      |> Enum.any?(&(&1 |> subset_unify(list2)))
  end

  @doc """
  computes a Stream of the resolvents from two clauses.
  """
  def list_resolvents(clause1, clause2) do # use renamings!!
    # IO.inspect clause1

    # IO.puts "---------- Clause2"
    # IO.inspect clause2

    factrs = literal_factors_stream(clause1)

    factrs
      |> Stream.flat_map(fn {[l|lits], u}->
        # IO.inspect {[l|lits], u}
        comp = complement(l |> apply_substitution(u))
        # IO.inspect comp
        # IO.puts "----------"
        unifying_literals_stream(clause2, comp)
          |> Stream.filter(&(length(&1)>0)) # no complement is found
          |> Stream.map( fn some_literals ->

            # IO.puts "---------- Some Literals"
            # IO.inspect some_literals
            {some_literals, mgu([comp| some_literals],u)}
          end)
          |> Stream.filter(fn {_some_literals, {res, _mg_unifier}} -> res end)
          |> Stream.map(fn {some_literals ,{_res, mg_unifier}} ->
            resolvent = apply_substitution((clause1--[l|lits])++(clause2--some_literals), mg_unifier)
            {[l|lits], some_literals, mg_unifier, resolvent }
          end)
      end)
  end


  @doc """
  Computes the resolvents of two clauses using only positive literals from clause1 and negative literals from clause2.
  """
  def partial_resolvents(clause1, clause2) do # use renamings!!
    %{positive: pclause1, negative: nclause1} = clause1
    %{positive: pclause2, negative: nclause2} = clause2
    list_resolvents(pclause1, nclause2)
      |> Enum.map(
        fn {_l1, _l2, mg_unifier, resolvent} ->
          new_clause(pclause2, resolvent++(nclause1 |> apply_substitution(mg_unifier)))
        end
      )
  end

  @doc """
  Computes the resolvents of two clauses.
  """
  def total_resolvents(clause1, clause2) do # use renamings!!
  partial_resolvents(clause1, clause2) ++ partial_resolvents(clause2, clause1)
  end


  def refutations(cnf), do: refutations(cnf,cnf)

  def refutations(cnf, []) do
    cnf
  end

  def refutations(cnf,[%{positive: [], negative: []}| _rest]) do
    cnf
  end

  def refutations(cnf, [c1| rest]) do
    res = cnf
            |> Stream.flat_map(&(total_resolvents(c1,&1)))
            |> Enum.map(fn e ->
                #IO.inspect e
                Kernel.elem(e, 3)
             end
             )
    # IO.inspect cnf
    # IO.inspect c1
    # IO.inspect res
    # IO.puts "----------"
    if Enum.member?(res, new_clause([])) do
      cnf++res
    else
      refutations(cnf++res, rest++res)
    end
  end

  # def factors([]), do: []

  # def factors([literal| rest_clause]) do
  #   (for y <- unifying_literals(rest_clause,literal), do: [literal|y]) ++ factors(rest_clause)
  # end

  # def factors(clause) do
  #   clause
  #     |> Enum.flat_map(&(unifying_literals(clause, &1)))
  # end

end


##
##
## q = Clauses.new_cqquery([Terms.new_var("x")], [Terms.new_term(:a, [Terms.new_var("x")]), Terms.new_term(:r, [Terms.new_var("x"), Terms.new_var("y")])])
## r = Clauses.new_erule([Terms.new_term(:b, [Terms.new_var("x")])], [Terms.new_term(:a, [Terms.new_var("x")]), Terms.new_term(:r, [Terms.new_var("x"), Terms.new_skterm(:sk1, [Terms.new_var("x")])])])
## Clauses.one_step_rewrite(q,r)
##
