


defprotocol ECompleto.Unification.Substitutions do
  @fallback_to_any true
  @spec apply_substitution(any, any) :: any
  def apply_substitution(term, s)
end

defprotocol ECompleto.Unification.Transform do
  @fallback_to_any true
  def transform_terms(term, function)
end

defmodule ECompleto.Unification do

  defimpl String.Chars, for: Map do
    def to_string(m) do
      args = m
        |> Enum.map(
          fn {k, v} ->
            "#{k}<-#{v |> String.Chars.to_string}"
          end
        )
        |> Enum.join(", ")
        "{#{args}}"
    end
  end

  # defimpl String.Chars, for: List do
  #   def to_string(clause) do
  #     args = clause
  #       |> Enum.map(&( &1 |> String.Chars.to_string ))
  #       |> Enum.join(", ")
  #     "[#{args}]"
  #   end
  # end

  # defimpl String.Chars, for: BitString do
  #   def to_string(m) do
  #     "m"
  #   end
  # end


  defimpl  ECompleto.Unification.Substitutions, for: ECompleto.Terms.Variable do
    def apply_substitution(term, s), do: Map.get(s, term |> String.Chars.to_string, term)
  end

  defimpl  ECompleto.Unification.Transform, for: ECompleto.Terms.Variable do
    def transform_terms(term, f), do: f.(term)
  end

  defimpl  ECompleto.Unification.Transform, for: ECompleto.Terms.FTerm do
    def transform_terms(term, f), do: f.(term)
  end


  defimpl  ECompleto.Unification.Substitutions, for: ECompleto.Terms.FTerm do
    def apply_substitution(term, s) do
      new_arguments = term.arguments |> Enum.map(
        fn t ->
          t |> ECompleto.Unification.Substitutions.apply_substitution(s)
        end
      )
      %{term | arguments: new_arguments}
    end
  end

  defimpl  ECompleto.Unification.Substitutions, for: ECompleto.Clauses.Atom do
    def apply_substitution(atom, s) do
      new_arguments = atom.arguments |> ECompleto.Unification.Substitutions.apply_substitution(s)
      %{atom | arguments: new_arguments}
    end
  end

  defimpl  ECompleto.Unification.Transform, for: ECompleto.Clauses.Atom do
    def transform_terms(atom, function) do
      new_arguments = atom.arguments |> ECompleto.Unification.Transform.transform_terms(function)
      %{atom | arguments: new_arguments}
    end
  end

  defimpl  ECompleto.Unification.Transform, for: List do
    def transform_terms(list, function) do
      list |> Enum.map(fn x -> x |> ECompleto.Unification.Transform.transform_terms(function) end)
    end
  end

  defimpl  ECompleto.Unification.Substitutions, for: ECompleto.Clauses.Clause do
    def apply_substitution(clause, s) do
      ECompleto.Clauses.new_clause(clause.positive |> ECompleto.Unification.Substitutions.apply_substitution(s),
              clause.negative |> ECompleto.Unification.Substitutions.apply_substitution(s))
    end
  end


  defimpl  ECompleto.Unification.Transform, for: ECompleto.Clauses.Clause do
    def transform_terms(clause, function) do
      ECompleto.Clauses.new_clause(clause.positive |>  ECompleto.Unification.Transform.transform_terms(function),
              clause.negative |> ECompleto.Unification.Transform.transform_terms(function))
    end
  end

  defimpl  ECompleto.Unification.Substitutions, for: ECompleto.Rules.ERule do
    def apply_substitution(rule, s) do
      %{rule |
          head: rule.head |> ECompleto.Unification.Substitutions.apply_substitution(s),
          body: rule.body |> ECompleto.Unification.Substitutions.apply_substitution(s),
          clauses: rule.clauses |> ECompleto.Unification.Substitutions.apply_substitution(s)
        }
    end
  end

  defimpl  ECompleto.Unification.Substitutions, for: List do
    def apply_substitution(l, s) do
      l |> Enum.map(
        fn t ->
          t |> ECompleto.Unification.Substitutions.apply_substitution(s)
        end
      )
    end
  end

  defimpl  ECompleto.Unification.Substitutions, for: Any do
    def apply_substitution(l, _s) do
      l
    end
  end


  import  ECompleto.Unification.Substitutions


  def compose(s1, s2) do
    s1
      |> Map.new(fn {k, v} -> {k, v |> apply_substitution(s2)} end)
      |> Map.merge(s2)
  end


  def new_substitution() do
    %{}
  end

  def new_substitution(other) do
    other |> Map.new(fn {k, v} -> {k |> String.Chars.to_string , v} end)
  end

  def contains?(term = %{}, t) do
    term
    |> Map.values
    |> Enum.any?(&(&1==t or contains?(&1,t)))
  end

  def contains?(term, t) when is_list(term) do
    term
    |> Enum.any?(&(&1==t or contains?(&1,t)))
  end

  def contains?(_term , _t) do
    false
  end


  # def unify(term1, term1, u) when is_atom(term1) do
  #   {true, u}
  # end

  def unify([], [], u) do
    {true, u}
  end

  def unify([x1 | r1], [x2 | r2], u) do
    # IO.inspect "List Term1 -------> #{Kernel.inspect(x1)}"
    # IO.inspect "List Term2 -------> #{Kernel.inspect(x2)}"

    # case unify(x1, x2, u) do
    #   {true, u_new} -> unify(
    #     Enum.map(r1, &(&1 |> apply_substitution(u_new))),
    #     Enum.map(r2, &(&1 |> apply_substitution(u_new))),
    #     u_new)
    #   _ -> {false , %{}}
    # end

    case unify(x1 |> apply_substitution(u), x2 |> apply_substitution(u), u) do
      {true, u_new} -> unify(r1, r2, u_new)
      _ -> {false , %{}}
    end

  end

  # def unify(term1=%{}, term2, u) when is_atom(term2) do
  #   t1 = Map.get(term1, :type)

  #   if t1== :variable do
  #     {true, compose_substitution(u, %{var_key(term1)=> term2})}
  #   else
  #     {false, %{}}
  #   end
  # end

  # def unify(term1, term2 = %{}, u) when is_atom(term1) do
  #   t2 = Map.get(term2, :type)

  #   if t2== :variable do
  #     {true, compose_substitution(u, %{var_key(term2)=> term1})}
  #   else
  #     {false, %{}}
  #   end
  # end

  def unify(term1=%{}, term2=%{}, u) do


    # IO.inspect "Map Term1 -------> #{Kernel.inspect(term1)}"
    # IO.inspect "Map Term2 -------> #{Kernel.inspect(term2)}"

    t1 = Map.get(term1, :type)
    t2 = Map.get(term2, :type)

    if t1== :variable do
      {!contains?(term2, term1), compose(u, %{String.Chars.to_string(term1)=> term2})}
    else
      if t2== :variable do
        {!contains?(term1, term2), compose(u, %{String.Chars.to_string(term2)=> term1})}
      else
        functors1 = term1
          |> Map.keys
          |> Enum.sort
        functors2 = term2
          |> Map.keys
          |> Enum.sort

        if functors1 == functors2 do
          unify(functors1 |> Enum.map(&( Map.get(term1, &1) )), functors1 |> Enum.map(&( Map.get(term2, &1) )), u)
        else
          {false , %{}}
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
  def mgu([t1, t2| rest], u) do
    {du, new_u} = unify(t1,t2,u)
    if du do
      [t2|rest]
        |> Enum.map(&( &1 |> apply_substitution(new_u)))
        |> mgu(new_u)
    else
      {false,new_substitution()}
    end
  end

  def mgu(l), do: mgu(l,new_substitution())



end
