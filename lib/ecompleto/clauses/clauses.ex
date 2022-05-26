defmodule ECompleto.Clauses do
  @moduledoc false
  import ECompleto.Formulas
  import ECompleto.Terms
  import ECompleto.Unification
  import ECompleto.Unification.Substitutions
  import Logger

  # alias ECompleto.Unification.apply_substitution
  alias ECompleto.Clauses.{
    Literal,
    Clause
  }

  alias ECompleto.Terms

  @doc """
  Creates an literal with its predicate, arguments and the sign (negated or not)
  ## Example
      iex(11)> new_literal("a",[new_var("X")])
      %ECompleto.Clauses.Literal{
        arguments: [%ECompleto.Terms.Variable{index: 0, name: "X", type: :variable}],
        key: {"a", 1},
        negated: false,
        predicate: "a",
        type: :atom
      }

  """
  @spec new_literal(String.t() | atom() | binary(), Terms.terms_list(), boolean()) :: Literal.t()
  def new_literal(predicate, arguments, negated \\ false) do
    %Literal{
      predicate: predicate,
      arguments: arguments,
      negated: negated,
      key: {predicate, arguments |> length}
    }
  end

  @doc """
  Creates a clause with a list of positive literals and a list of negative literals
  ## Example
    iex(15)> c = new_clause([new_literal("a",[new_var("X")])],  [new_literal("a",[new_var("Y")], true)])
    %ECompleto.Clauses.Clause{
      negative: [
        %ECompleto.Clauses.Literal{
          arguments: [
            %ECompleto.Terms.Variable{index: 0, name: "Y", type: :variable}
          ],
          key: {"a", 1},
          negated: true,
          predicate: "a",
          type: :atom
        }
      ],
      negative_frozen: [
        %ECompleto.Clauses.Literal{
          arguments: [
            %ECompleto.Terms.FTerm{
              arguments: [],
              functor: "Y'",
              skolem: false,
              type: :term
            }
          ],
          key: {"a", 1},
          negated: true,
          predicate: "a",
          type: :atom
        }
      ],
      negative_keys: #MapSet<[{"a", 1}]>,
      positive: [
        %ECompleto.Clauses.Literal{
          arguments: [
            %ECompleto.Terms.Variable{index: 0, name: "X", type: :variable}
          ],
          key: {"a", 1},
          negated: false,
          predicate: "a",
          type: :atom
        }
      ],
      positive_frozen: [
        %ECompleto.Clauses.Literal{
          arguments: [
            %ECompleto.Terms.FTerm{
              arguments: [],
              functor: "X'",
              skolem: false,
              type: :term
            }
          ],
          key: {"a", 1},
          negated: false,
          predicate: "a",
          type: :atom
        }
      ],
      positive_keys: #MapSet<[{"a", 1}]>,
      type: :clause
      }

  """
  @spec new_clause(Literal.literals(), Literal.literals()) :: Clause.t()
  def new_clause(pliterals, nliterals) do
    p = pliterals |> Enum.uniq()
    n = nliterals |> Enum.uniq()

    %Clause{
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
  @spec new_clause_mix(Literal.literals()) :: Clause.t()
  def new_clause_mix(literals) do
    new_clause(
      literals |> Enum.filter(&is_positive?(&1)),
      literals |> Enum.filter(&is_negative?(&1))
    )
  end

  @doc """
  if the clause has only one positive literal or only one negative literal
  """
  @spec unit_clause?(Clause.t()) :: boolean()
  def unit_clause?(clause) do
    case {clause.positive, clause.negative} do
      {[], [_l]} -> true
      {[_l], []} -> true
      _ -> false
    end
  end

  @doc """
  if the clause has only negative literals
  """
  @spec constraint_clause?(Clause.t()) :: boolean()
  def constraint_clause?(clause) do
    case {clause.positive, clause.negative} do
      {[], [_l | _rest]} -> true
      _ -> false
    end
  end

  @doc """
  Tells if a literal is negative.
  """
  @spec is_negative?(Literal.t()) :: boolean()
  def is_negative?(literal = %Literal{}) do
    literal.negated
  end

  @doc """
  Tells if a literal is positive.
  """
  @spec is_positive?(Literal.t()) :: boolean()
  def is_positive?(literal = %Literal{}) do
    not is_negative?(literal)
  end

  @doc """
  Tells if a literal is an answer atom, i.e., the predicate is equal to `:answer_atom`.
  """
  @spec is_answer_literal(Literal.t()) :: boolean()
  def is_answer_literal(literal = %Literal{}) do
    literal.predicate == :answer_atom
  end

  @doc """
  Gets the complement of a literal.
  """
  @spec complement(Literal.t()) :: Literal.t()
  def complement(f) do
    %{f | negated: not f.negated}
  end

  @doc """
  Returns a stream of all the subsets of the list of literals that unify with a specified literal.
  """
  @spec unifying_literals_stream(Literal.literals(), Literal.t()) :: Enumerable.t()
  defp unifying_literals_stream(literals_list, literal) do
    literals_list
    |> Enum.filter(
      &(&1
        |> unify(literal, %{})
        |> Kernel.elem(0))
    )
    |> ECompleto.Utils.subsets_stream()
  end

  @doc """
  Returns a stream of mgus of the literals that unify with a specified literal.
  """
  @spec unifying_single_literals_stream(Literal.literals(), Literal.t()) :: Enumerable.t()
  defp unifying_single_literals_stream(literals_list, literal) do
    literals_list
    |> Enum.map(fn l -> l |> unify(literal, %{}) end)
    |> Enum.filter(fn {uni, _mg} -> uni end)
    |> Enum.map(fn {_uni, mg} -> mg end)
  end

  @doc """
  Returns a stream of literals and mgu pairs that unify with a specified literal.
  """
  @spec unifying_single_literals_mgu_stream(Literal.literals(), Literal.t()) :: Enumerable.t()
  def unifying_single_literals_mgu_stream(literals_list, literal) do
    literals_list
    |> Stream.map(fn l -> l |> unify(literal, %{}) end)
    |> Stream.filter(fn {uni, _mg} -> uni end)
  end

  defp unifying_literals(literals_list, literal) do
    literals_list
    |> Enum.filter(
      &(&1
        |> unify(literal, %{})
        |> Kernel.elem(0))
    )
  end

  @doc """
  unifies the answer literals in a list of literals.
  """
  @spec unify_answer_literals(Literal.literals()) :: Literal.literals()
  def unify_answer_literals(literals_list) do
    al =
      literals_list
      |> Enum.filter(fn l -> l |> is_answer_literal end)

    {true, mg} = mgu(al)

    if al |> length > 1 do
      [al |> List.first() | literals_list -- al] |> apply_substitution(mg)
    else
      literals_list
    end
  end

  @doc """
  gives a stream of all the factos of a list of literals.
  """
  defp literal_factors_stream([]), do: []

  defp literal_factors_stream([literal | rest_clause]) do
    rest_clause
    |> unifying_literals_stream(literal)
    |> Stream.map(fn some_literals ->
      {[literal | some_literals], mgu([literal | some_literals])}
    end)
    |> Stream.filter(fn {_some_literals, {res, _mg_unifier}} -> res end)
    |> Stream.map(fn {some_literals, {_res, mg_unifier}} -> {some_literals, mg_unifier} end)
    |> Stream.concat(rest_clause |> literal_factors_stream)
  end

  @doc """
  Skolemizes the existential variables of a rule by replacing them with unique (local to the rule)
  Skolem functions. As long as we do rewritings with respect to clauses that do not contain Skolem terms this
  should be fine. However, in Chase algorithms we need to ensure that the nulls created have a globally unique
  function symbol.
  """
  @spec skolemize(Literal.literals(), Literal.literals()) :: Literal.literals()
  def skolemize(head, body) do
    existential_vars =
      head
      |> iterate_subterms()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :type) == :variable and !contains?(body, &1)))
      |> Enum.sort()
      |> Enum.uniq()

    body_vars =
      body
      |> iterate_subterms()
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :type) == :variable))
      |> Enum.sort()
      |> Enum.uniq()

    head =
      existential_vars
      |> Enum.reduce(
        head,
        fn v, acc ->
          acc |> apply_substitution(new_substitution([{v, new_skterm(v.name, body_vars)}]))
        end
      )

    head
  end

  @doc """
  checks if clause1 subsumes clause2, i.e., clause2 can be dediced from clause1.
  """
  def subsumes(clause1, clause2) do
    if clause1.positive_keys |> MapSet.subset?(clause2.positive_keys) and
         clause1.negative_keys |> MapSet.subset?(clause2.negative_keys) do
      subset_unify(clause1.positive, clause2.positive_frozen) and
        subset_unify(clause1.negative, clause2.negative_frozen)
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

  def subset_unify([literal | rest1], list2) do
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
  # use renamings!!
  def list_resolvents(clause1, clause2) do
    # IO.inspect clause1

    # IO.puts "---------- Clause2"
    # IO.inspect clause2

    factrs = literal_factors_stream(clause1)

    factrs
    |> Stream.flat_map(fn {[l | lits], u} ->
      # IO.inspect {[l|lits], u}
      comp = complement(l |> apply_substitution(u))
      # IO.inspect comp
      # IO.puts "----------"
      unifying_literals_stream(clause2, comp)
      # no complement is found
      |> Stream.filter(&(length(&1) > 0))
      |> Stream.map(fn some_literals ->
        # IO.puts "---------- Some Literals"
        # IO.inspect some_literals
        {some_literals, mgu([comp | some_literals], u)}
      end)
      |> Stream.filter(fn {_some_literals, {res, _mg_unifier}} -> res end)
      |> Stream.map(fn {some_literals, {_res, mg_unifier}} ->
        resolvent = apply_substitution((clause1 -- [l | lits]) ++ (clause2 -- some_literals), mg_unifier)

        {[l | lits], some_literals, mg_unifier, resolvent}
      end)
    end)
  end

  @doc """
  Computes the resolvents of two clauses using only positive literals from clause1 and negative literals from clause2.
  """
  # use renamings!!
  def partial_resolvents(clause1, clause2) do
    %{positive: pclause1, negative: nclause1} = clause1
    %{positive: pclause2, negative: nclause2} = clause2

    list_resolvents(pclause1, nclause2)
    |> Enum.map(fn {_l1, _l2, mg_unifier, resolvent} ->
      new_clause(pclause2, resolvent ++ (nclause1 |> apply_substitution(mg_unifier)))
    end)
  end

  @doc """
  Computes the resolvents of two clauses.
  """
  # use renamings!!
  def total_resolvents(clause1, clause2) do
    partial_resolvents(clause1, clause2) ++ partial_resolvents(clause2, clause1)
  end

  def refutations(cnf, [], [], filter) do
    Logger.debug("Fix point !!!!!!!!!!!!!")
    {false, cnf}
  end

  def refutations(cnf, [_x | _generated], [], filter) do
    Logger.debug("Start Resolution Again!!!!!!!!!!!!!")
    refutations(cnf, [], cnf |> Enum.filter(filter), filter)
  end

  # def refutations(cnf, [%{positive: [], negative: []} | _rest], filter) do
  #   cnf
  # end

  def refutations(cnf, generated, [c1 | rest], filter) do
    res =
      cnf
      |> Enum.flat_map(&total_resolvents(c1, &1))
      |> Enum.filter(fn c ->
        cnf
        |> Enum.all?(fn c2 ->
          not (c2 |> subsumes(c) and c |> subsumes(c2))
        end)
      end)

    new_generated = generated ++ res
    Logger.debug("Using #{c1} for resolution")
    Logger.debug("Using clauses {#{cnf |> Enum.join(", ")}} for resolution")
    Logger.debug("Generated #{res |> length} resolvents, total #{new_generated |> length}")
    Logger.debug("Generated {#{res |> Enum.join(", ")}}")

    if Enum.member?(res, new_clause([], [])) do
      Logger.debug("Empty Clause Found")
      {true, cnf ++ res}
    else
      refutations(cnf ++ res, new_generated, rest ++ (res |> Enum.filter(filter)), filter)
    end
  end

  ## ECompleto.Program.load_program("examples/q_cont.dlgp") |> ECompleto.Program.program_to_cnf |> ECompleto.Clauses.refutations_unit

  def refutations(cnf, filter), do: refutations(cnf, [], cnf |> Enum.filter(filter), filter)
  def refutations(cnf), do: refutations(cnf, fn _clause -> true end)

  @doc """
  todo check if factorization is creating all possible unit clauses or not 
  """
  def refutations_unit(cnf), do: refutations(cnf, fn clause -> clause |> unit_clause? end)

  def refutations_constraint(cnf), do: refutations(cnf, fn clause -> clause |> constraint_clause? end)

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
