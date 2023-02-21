defmodule ECompleto.Formulas do
  @moduledoc false

  import ECompleto.Terms
  import ECompleto.Unification
  import ECompleto.Unification.Substitutions

  alias ECompleto.Clauses.{
    Literal,
    Clause
  }

  alias ECompleto.Rules.{
    ERule,
    DERule
  }

  alias ECompleto.Queries.CQuery

  @type formula ::
          Literal.t() | Clause.t() | ERule.t() | DERule.t() | CQuery.t()
  @type iterable_term ::
          [formula()]
          | formula()

  @doc """
  iterates through the subformulas, terms and subterms of a formula.
  """
  @spec iterate_subterms(iterable_term()) :: Enumerable.t()
  def iterate_subterms(term) when is_list(term) do
    term
    |> Stream.flat_map(fn x -> iterate_subterms(x) end)
  end

  def iterate_subterms(term = %{}) do
    type = Map.get(term, :type)
    # IO.inspect(type)
    term_keys =
      cond do
        type == :clause -> [:negative, :positive]
        type in [:atom, :term] -> [:arguments]
        type in [:erule, :drule] -> [:clauses, :head, :body]
        type == :cquery -> [:clauses, :answer_tuple, :body]
        true -> []
      end

    term_keys
    |> Stream.flat_map(fn x -> Map.get(term, x) |> iterate_subterms end)
    |> Stream.concat([term])
  end

  def iterate_subterms(term) do
    Stream.concat([term])
  end

  @doc """
  renames two formulas so that they dont share the same variables.
  """
  @spec rename_appart(iterable_term(), iterable_term()) :: {iterable_term(), iterable_term()}
  def rename_appart(term1, term2) do
    fr1 = term1 |> max_indexes(%{})
    fr1 = term2 |> max_indexes(fr1)
    renaming = term2 |> build_renaming(fr1)
    {term1, term2 |> apply_substitution(renaming)}
  end

  @doc """
  buils a renaming substitution for a term such that the indexes of the variables start from a specified minimum value.
  """
  @spec build_renaming(iterable_term(), map()) :: map()
  def build_renaming(term, fr0) do
    vars =
      term
      |> iterate_subterms
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :type) == :variable))
      |> Enum.uniq()
      |> Enum.sort()

    maps =
      vars
      |> Enum.map(fn var ->
        {var |> String.Chars.to_string(), ECompleto.Terms.new_var(var.name, var.index + (fr0 |> Map.get(var.name)))}
      end)

    Map.new(maps)
  end

  @doc """
  Renames the variables of a term with a consecutive index starting from a minimum value specified in fr0.
  """
  @spec rename(iterable_term(), map()) :: {iterable_term(), map()}
  def rename(term, fr0 \\ %{}) do
    vars =
      term
      |> iterate_subterms
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :type) == :variable))
      |> Enum.uniq()
      |> Enum.sort()

    ## Builds a map with the variable names and the indexes in the term
    maps =
      vars
      |> Enum.group_by(&Map.get(&1, :name), &Map.get(&1, :index))

    ## Builds a map with the variable names and maximum indexes in the resulting renamed term
    fr =
      vars
      |> Enum.frequencies_by(&Map.get(&1, :name))
      |> Map.new(fn {k, f} ->
        {k, f + Map.get(fr0, k, 0)}
      end)
      |> Map.merge(fr0, fn _k, v1, _v2 ->
        v1
      end)

    {rename_aux(maps, term, fr0), fr}
  end

  # computes the maximum indexes that the variables have in a formula.
  defp max_indexes(term, initial) do
    vars =
      term
      |> iterate_subterms
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :type) == :variable))
      |> Enum.uniq()
      |> Enum.sort()

    vars
    |> Enum.group_by(&Map.get(&1, :name), &Map.get(&1, :index))
    |> Map.new(fn {name, indexes} ->
      {
        name,
        indexes
        |> Enum.reduce(
          initial |> Map.get(name, 0),
          fn x, acc -> max(x + 1, acc) end
        )
      }
    end)
    |> Map.merge(initial, fn _k, v1, _v2 ->
      v1
    end)
  end

  def vars_to_term(term) do
    case term do
      %{} ->
        type = Map.get(term, :type)

        if type == :variable do
          new_term("#{term}'", [])
        else
          term
        end

      _ ->
        term
    end
  end

  @doc """
  Turns the variables into constants.
  """
  @spec freeze(Literal.literals() | Literal.t()) :: Literal.literals() | Literal.t()
  def freeze(literals) do
    literals
    |> ECompleto.Unification.Transform.transform_terms(&vars_to_term(&1))
  end

  def rename_aux(maps, term, pos0) do
    maps
    |> Enum.reduce(
      term,
      fn {k, v}, acc ->
        {res1, _} =
          v
          |> Enum.reduce(
            {acc, Map.get(pos0, k, 0)},
            fn i, {acc1, pos} ->
              {
                apply_substitution(
                  acc1,
                  new_substitution([{new_var(k, i), new_var(k, pos)}])
                ),
                pos + 1
              }
            end
          )

        res1
      end
    )
  end
end
