defprotocol ECompleto.Unification.Substitutions do
  @fallback_to_any true
  @spec apply_substitution(any, any) :: any
  @doc """
  applies a substitution to a formula or term.
  """
  def apply_substitution(term, s)
end

defprotocol ECompleto.Unification.Transform do
  @fallback_to_any true
  @doc """
  applies a transformation function to a formula or a term.
  """
  def transform_terms(term, function)
end

defmodule ECompleto.Unification do
  @moduledoc false
  defimpl String.Chars, for: Map do
    def to_string(m) do
      args =
        m
        |> Enum.map_join(", ", fn {k, v} ->
          "#{k}<-#{v |> String.Chars.to_string()}"
        end)

      "{#{args}}"
    end
  end

  defimpl ECompleto.Unification.Substitutions, for: ECompleto.Terms.Variable do
    def apply_substitution(term, s), do: Map.get(s, term |> String.Chars.to_string(), term)
  end

  defimpl ECompleto.Unification.Transform, for: ECompleto.Terms.Variable do
    def transform_terms(term, f), do: f.(term)
  end

  defimpl ECompleto.Unification.Transform, for: ECompleto.Terms.FTerm do
    def transform_terms(term, f), do: f.(term)
  end

  defimpl ECompleto.Unification.Substitutions, for: ECompleto.Terms.FTerm do
    def apply_substitution(term, s) do
      new_arguments =
        term.arguments
        |> Enum.map(fn t ->
          t |> ECompleto.Unification.Substitutions.apply_substitution(s)
        end)

      %{term | arguments: new_arguments}
    end
  end

  defimpl ECompleto.Unification.Substitutions, for: ECompleto.Clauses.Literal do
    def apply_substitution(atom, s) do
      new_arguments = atom.arguments |> ECompleto.Unification.Substitutions.apply_substitution(s)
      %{atom | arguments: new_arguments}
    end
  end

  defimpl ECompleto.Unification.Transform, for: ECompleto.Clauses.Literal do
    def transform_terms(atom, function) do
      new_arguments = atom.arguments |> ECompleto.Unification.Transform.transform_terms(function)
      ECompleto.Clauses.new_literal(function.(atom.predicate), new_arguments, atom.negated)
    end
  end

  defimpl ECompleto.Unification.Transform, for: List do
    def transform_terms(list, function) do
      list |> Enum.map(fn x -> x |> ECompleto.Unification.Transform.transform_terms(function) end)
    end
  end

  defimpl ECompleto.Unification.Substitutions, for: ECompleto.Clauses.Clause do
    def apply_substitution(clause, s) do
      ECompleto.Clauses.new_clause(
        clause.positive |> ECompleto.Unification.Substitutions.apply_substitution(s),
        clause.negative |> ECompleto.Unification.Substitutions.apply_substitution(s)
      )
    end
  end

  defimpl ECompleto.Unification.Transform, for: ECompleto.Clauses.Clause do
    def transform_terms(clause, function) do
      ECompleto.Clauses.new_clause(
        clause.positive |> ECompleto.Unification.Transform.transform_terms(function),
        clause.negative |> ECompleto.Unification.Transform.transform_terms(function)
      )
    end
  end

  defimpl ECompleto.Unification.Transform, for: ECompleto.Program do
    def transform_terms(prog, function) do
      ECompleto.Program.new_program(
        prog.headers,
        prog.body |> ECompleto.Unification.Transform.transform_terms(function)
      )
    end
  end

  defimpl ECompleto.Unification.Transform, for: ECompleto.Rules.ERule do
    def transform_terms(rule, function) do
      ECompleto.Rules.new_erule(
        rule.head |> ECompleto.Unification.Transform.transform_terms(function),
        rule.body |> ECompleto.Unification.Transform.transform_terms(function)
      )
    end
  end

  defimpl ECompleto.Unification.Transform, for: ECompleto.Rules.DERule do
    def transform_terms(rule, function) do
      ECompleto.Rules.new_derule(
        rule.head |> ECompleto.Unification.Transform.transform_terms(function),
        rule.body |> ECompleto.Unification.Transform.transform_terms(function)
      )
    end
  end

  defimpl ECompleto.Unification.Transform, for: ECompleto.Queries.CQuery do
    def transform_terms(q, function) do
      ECompleto.Queries.new_cquery(
        q.answer_tuple |> ECompleto.Unification.Transform.transform_terms(function),
        q.body |> ECompleto.Unification.Transform.transform_terms(function)
      )
    end
  end

  defimpl ECompleto.Unification.Substitutions, for: ECompleto.Rules.ERule do
    def apply_substitution(rule, s) do
      %{
        rule
        | head: rule.head |> ECompleto.Unification.Substitutions.apply_substitution(s),
          body: rule.body |> ECompleto.Unification.Substitutions.apply_substitution(s),
          clauses: rule.clauses |> ECompleto.Unification.Substitutions.apply_substitution(s)
      }
    end
  end

  defimpl ECompleto.Unification.Substitutions, for: List do
    def apply_substitution(l, s) do
      l
      |> Enum.map(fn t ->
        t |> ECompleto.Unification.Substitutions.apply_substitution(s)
      end)
    end
  end

  defimpl ECompleto.Unification.Substitutions, for: Any do
    def apply_substitution(l, _s) do
      l
    end
  end

  import ECompleto.Unification.Substitutions

  @doc """
  builds the composition of two substitutions, i.e., s1(s2)
  """
  def compose(s1, s2) do
    s1
    |> Map.new(fn {k, v} -> {k, v |> apply_substitution(s2)} end)
    |> Map.merge(s2)
  end

  @doc """
  creates an empty substitution.
  """
  def new_substitution() do
    %{}
  end

  @doc """
  creates a substitution based on the mappings of another substitution.
  """
  def new_substitution(other) do
    other |> Map.new(fn {k, v} -> {k |> String.Chars.to_string(), v} end)
  end

  @doc """
  checks if a term contains another term.
  """
  def contains?(term = %{}, t) do
    term
    |> Map.values()
    |> Enum.any?(&(&1 == t or contains?(&1, t)))
  end

  def contains?(term, t) when is_list(term) do
    term
    |> Enum.any?(&(&1 == t or contains?(&1, t)))
  end

  def contains?(_term, _t) do
    false
  end

  def unify([], [], u) do
    {true, u}
  end

  def unify([x1 | r1], [x2 | r2], u) do
    case unify(x1 |> apply_substitution(u), x2 |> apply_substitution(u), u) do
      {true, u_new} -> unify(r1, r2, u_new)
      _ -> {false, %{}}
    end
  end

  @doc """
  checks if two terms unify with respect to a unifier.
  """
  def unify(term1 = %{}, term2 = %{}, u) do
    t1 = Map.get(term1, :type)
    t2 = Map.get(term2, :type)

    if t1 == :variable do
      {!contains?(term2, term1), compose(u, %{String.Chars.to_string(term1) => term2})}
    else
      if t2 == :variable do
        {!contains?(term1, term2), compose(u, %{String.Chars.to_string(term2) => term1})}
      else
        functors1 =
          term1
          |> Map.keys()
          |> Enum.sort()

        functors2 =
          term2
          |> Map.keys()
          |> Enum.sort()

        if functors1 == functors2 do
          unify(
            functors1 |> Enum.map(&Map.get(term1, &1)),
            functors1 |> Enum.map(&Map.get(term2, &1)),
            u
          )
        else
          {false, %{}}
        end
      end
    end
  end

  def unify(term1, term1, u) do
    {true, u}
  end

  def unify(_, _, _) do
    {false, %{}}
  end

  def mgu([], u), do: {true, u}
  def mgu([_term], u), do: {true, u}

  @doc """
  finds the most general unifier (mgu) of the formulas in a list  with respect to a substitution.
  """
  def mgu([t1, t2 | rest], u) do
    {du, new_u} = unify(t1, t2, u)

    if du do
      [t2 | rest]
      |> Enum.map(&(&1 |> apply_substitution(new_u)))
      |> mgu(new_u)
    else
      {false, new_substitution()}
    end
  end

  @doc """
  finds the most general unifier (mgu) of the formulas in a list.
  """
  def mgu(l), do: mgu(l, new_substitution())
end
