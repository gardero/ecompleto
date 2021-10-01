
import ECompleto.Clauses
import ECompleto.Unification
import ECompleto.Unification.Substitutions
import ECompleto.Terms
require Logger

defmodule ECompleto.Rules.ERule do
  @doc """
  Defines an Existential Rule with a body and a head.
  """
  defstruct [body: [], head: [], clauses: [], type: :erule, alias: ""]


  defimpl String.Chars, for: ECompleto.Rules.ERule do
    def to_string(rule) do
      abody = rule.body
        |> Enum.map(&( &1 |> String.Chars.to_string ))
        |> Enum.join(", ")

      ahead = rule.head
        |> Enum.map(&( &1 |> String.Chars.to_string ))
        |> Enum.join(", ")

      if rule.head |> length > 0 do
        if rule.body |> length > 0 do
          "#{ahead} :- #{abody}."
        else
          "#{ahead}."
        end
      else
        "! :- #{abody}."
      end
    end
  end

end



defmodule ECompleto.Rules.DERule do
  @doc """
  Defines an Existential Rule with a body and a head.
  """
  defstruct [body: [], head: [], clauses: [], type: :derule, alias: ""]

  defimpl String.Chars, for: ECompleto.Rules.DERule do
    def to_string(rule) do
      abody = rule.body
        |> Enum.map(&( &1 |> String.Chars.to_string ))
        |> Enum.join(", ")

      ahead = rule.head
        |> Enum.map( fn hi ->
          thi = hi |> Enum.map(fn l -> "#{l}" end) |> Enum.join(", ")
          if hi |> length > 1 do
            "(#{thi})"
          else
            thi
          end
        end )
        |> Enum.join(", ")

      if rule.head |> length > 0 do
        "[#{ahead}] :- #{abody}."
      else
        "! :- #{abody}."
      end
    end
  end

end



defmodule ECompleto.Rules do

  @doc """
  creates a new existential rule with a specified head and body.
  """
  def new_erule(head, body) do

    head = skolemize(head, body)
    # builds clauses assiated to the rule.
    b = body |> Enum.map(&(complement(&1)))
    c = if head |> length > 0 do
      head |> Enum.map(
        fn hi ->
          new_clause([hi], b)
        end
        )
    else
     [new_clause([],b)]
    end

    # renames the clauses to ensure they dont share variables.
    {c, _x} = Enum.map_reduce(c, %{}, fn ci, frs ->
      # IO.inspect frs
      rename(ci, frs)
    end)

    # IO.inspect c |> Enum.map(&(String.Chars.to_string(&1)))

    %ECompleto.Rules.ERule{
      head: head,
      body: body,
      clauses: c
    }

  end


  @doc """
  creates a new disjunctive existential rule with a specified head and body.
  """
  def new_derule(head, body) do

    head = head |> Enum.map(
      fn hi ->
        skolemize(hi, body)
      end
    )
    # for now we are not building the clase representation.
    c = []
    %ECompleto.Rules.DERule{
      head: head,
      body: body,
      clauses: c
    }

  end

  def one_step_rewrite(q=%ECompleto.Queries.CQuery{}, rule) do
    one_step_rewrite(q.clauses |> List.first, rule)
  end

  def one_step_rewrite(cc, rule=%ECompleto.Rules.ERule{}) do
    # fist we do a fast check to see if the clause contains any of the predicates in the head of the rule.
    if (Enum.any?(rule.head, fn a -> a.key in cc.negative_keys end)) do
      # we then ansure that the clause does not share a variable with the clauses in or rule.
      {rule, cc} = rename_appart(rule, cc)
      # then we apply all possible resolution steps with respect to the clauses of the rules and
      # keep the ones that do not leave skollem terms in the result.
      %{clauses: cl} = rule
      [{cc, [],%{}}]
        |> one_step_rewrite_aux(cl)
        |> Enum.filter(
          fn {c, new_atoms, _mg_unifier} ->
            c != cc and !(c |> iterate_subterms |> Enum.any?(&( &1 |> is_skterm? ))) and !(new_atoms |> iterate_subterms |> Enum.any?(&( &1 |> is_skterm? )))
          end
        )
        |> Enum.map(
          # then we build the clauses and unify the answer literals.
          fn {c, new_atoms, _mg_unifier} ->
            %{positive: pc, negative: nc} = c
            new_clause(pc, (nc++new_atoms) |> unify_answer_literals)
          end
        )
    else
      []
    end
  end

  @doc """
  rewrites (one step) a query (or a clause) with respect to a list of existential rules.
  Each rule is used in parallel then the results are combined.
  The idea here is to use clauses that has no possitive literals.
  """
  def one_step_rewrite(cc, rules) when is_list(rules) do
    async_rewrite = fn(r) ->
      caller = self()
      spawn(fn ->
        send(caller, {:result, one_step_rewrite(cc, r)})
      end)
    end

    get_result = fn ->
      receive do
      {:result, result} -> result
      end
    end

    rules |> Enum.map(&async_rewrite.(&1))
    |> Enum.map(fn(_) -> get_result.() end)
    |> Enum.flat_map(
          fn rw ->
            rw
          end
        )
  end

  ## {new_cover, added, removed} = new_cc
  ## |> most_general(ucq)
  def one_step_rewrite_cover(cc, rules, ucq) when is_list(rules) do
    async_rewrite = fn(r) ->
      caller = self()
      spawn(fn ->
        send(caller, {:result, one_step_rewrite(cc, r)})
      end)
    end

    get_result = fn ->
      receive do
      {:result, result} -> result
      end
    end

    rules |> Enum.map(&async_rewrite.(&1))
    |> Enum.map(fn(_) -> get_result.() end)
    |> Enum.reduce({ucq, [], []}, fn rw, {new_cover, added, removed} ->
      {new_cover1, added1, removed1} = rw
      |> ECompleto.Rewriting.most_general(new_cover)
      rem = removed++removed1
      {new_cover1, (added++added1) -- rem, rem}
    end)
  end




  def one_step_rewrite_aux(ucq,[]) do
    ucq
  end

  def one_step_rewrite_aux(ucq, [rule_clause| rest]) do
    %{positive: pclause1, negative: nclause1} = rule_clause
    res = ucq
            |> Enum.flat_map(fn {constraint_clause, new_atoms, _u} ->
                #IO.inspect e
                %{positive: pclause2, negative: nclause2} = constraint_clause
                # IO.inspect rule_clause |> String.Chars.to_string
                # IO.inspect constraint_clause |> String.Chars.to_string

                list_resolvents(pclause1, nclause2)
                  |> Enum.map(
                    fn {_l1, _l2, mg_unifier, resolvent} ->
                      if new_atoms == [] do
                        {
                          new_clause(pclause2, resolvent),
                          nclause1 |> apply_substitution(mg_unifier),
                          mg_unifier
                        }
                      else
                        {
                          new_clause(pclause2, resolvent),
                          new_atoms |> apply_substitution(mg_unifier),
                          mg_unifier
                          }
                      end
                    end
                  )
             end
             )
    one_step_rewrite_aux(ucq++res, rest)
  end



  # def one_step_rewrite(cc, rules) when is_list(rules) do
  #   rules
  #     |> Enum.flat_map(
  #       fn r ->
  #         one_step_rewrite(cc, r)
  #       end
  #     )
  # end

  @doc """
  rewrites (one step) a clause with respect to a disjunctive existential rule.
  The idea here is to use clauses that has no possitive literals.
  """
  def one_step_drewrite(cc, rule=%ECompleto.Rules.DERule{}) do
    {rule, cc} = rename_appart(rule, cc)

    Logger.debug("Using CC #{cc}")
    Logger.debug("Rewrite DER #{rule}")

    ## IMPORTANT CC has no possitive part

    rw = rule.head |> Enum.flat_map(
      fn hi ->
        if (hi |> Enum.any?(fn a -> a.key in cc.negative_keys end)) do
          new_head = rule.head -- [hi]
          aux_rule = new_erule(hi, rule.body)
          %{clauses: cl} = aux_rule

          # Logger.debug("Aux Rule #{aux_rule}")

          [{cc, [],%{}}]
          |> one_step_rewrite_aux(cl)
          |> Enum.filter(fn {c, new_atoms, _s} ->
            !([c, new_atoms] |> iterate_subterms
            |> Enum.any?(&( &1 |> is_skterm?))) and c != cc
          end)
          |> Enum.map(
            fn {c, new_atoms, mg_unifier} ->
              %{positive: _pc, negative: nc} = c

              # IO.inspect c |> String.Chars.to_string
              # IO.inspect new_atoms |> Enum.map(&("#{&1}")) |> Enum.join(", ")
              # IO.inspect nc |> Enum.map(&("#{&1}")) |> Enum.join(", ")
              res = new_derule(new_head |> apply_substitution(mg_unifier), (nc++new_atoms) |> Enum.map(fn b -> b|> complement end) |> unify_answer_literals)
              # Logger.debug("New Rule #{res}")
              res
            end
          )
        else
          []
        end
      end
    )
    |> Enum.group_by(
      fn r -> r.head |> length == 1 end,
      fn x -> x end
    )
    {
      rw |> Map.get(true,[]) |> Enum.map(
      fn r ->
        new_erule(r.head |> List.first, r.body)
      end
      ),
      rw |> Map.get(false,[])
    }
  end






end
