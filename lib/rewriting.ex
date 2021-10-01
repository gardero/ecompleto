

defmodule ECompleto.Rewriting do

  import ECompleto.Rules.ERule
  import ECompleto.Rules
  import ECompleto.Clauses
  require Logger

  def rewrite(program=%ECompleto.Program{}) do
    {constraint_clauses, _, _} = (program.queries |> Enum.flat_map(&(&1.clauses))) ++ (program.constraints |> Enum.flat_map(&(&1.clauses)))
      |> most_general([])
      Logger.info("CQs #{constraint_clauses|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")
      Logger.info("Rules #{program.rules|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")
      Logger.info("DRules #{program.disj_rules|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")

    rewrite_disj_queue(constraint_clauses, constraint_clauses, [], program.rules, program.disj_rules)
  end

  def erewrite(program=%ECompleto.Program{}) do
    {constraint_clauses, _, _} = (program.queries |> Enum.flat_map(&(&1.clauses))) #++ (program.constraints |> Enum.flat_map(&(&1.clauses)))
      |> most_general([])
    rewrite_queue(constraint_clauses, constraint_clauses, program.rules)
  end

  def rewrite_queue([], ucq, _rules) do
    ucq
  end

  def rewrite_queue1([cc|queue], ucq, rules) do
    new_cc = cc |> one_step_rewrite(rules)
    {ucq, queue} = new_cc |> cover(ucq, queue)
    # IO.puts "================="
    # IO.puts "Queue"
    # IO.puts queue |> Enum.map(&("#{&1}"))
    # IO.puts "Cover"
    # IO.puts ucq |> Enum.map(&("#{&1}"))
    # :timer.sleep(30000)
    rewrite_queue(queue, ucq, rules)
  end

  def rewrite_queue([cc|queue], ucq, rules) do
    {new_cover, added, removed} = cc |> one_step_rewrite_cover(rules, ucq)
    {ucq, queue} = {new_cover, (queue -- removed) ++ added}
    rewrite_queue(queue, ucq, rules)
  end


  def rewrite_queue_one_step(_, new_queue, ucq, []) do
    {ucq, new_queue}
  end

  def rewrite_queue_one_step([], new_queue, ucq, _rules) do
    {ucq, new_queue}
  end

  def rewrite_queue_one_step([cc|queue], new_queue, ucq, rules) do
    new_cc = cc |> one_step_rewrite(rules)
    # IO.inspect("COVER #{ucq |> length}+#{new_cc|> length}")
    {new_cover, added, removed} = new_cc
      |> most_general(ucq)
    # IO.inspect("NEW COVER #{new_cover |> length}+#{added|> length}-#{removed |> length}")
    ucq = new_cover
    queue = queue -- removed
    new_queue = (new_queue -- removed) ++ added
    rewrite_queue_one_step(queue, new_queue, ucq, rules)
  end

  def rewrite_async_queue_one_step(queue, new_queue, ucq, rules) do
    async_rewrite = fn(cc) ->
      caller = self
      spawn(fn ->
        new_cc = cc |> one_step_rewrite(rules)
      send(caller, {:result, new_cc})
      end)
    end

    get_result = fn ->
      receive do
      {:result, result} -> result
      end
    end
    {ucq, added, removed} = queue |> Enum.map(&async_rewrite.(&1))
    |> Enum.map(fn(_) -> get_result.() end)
    |> Enum.reduce({ucq, [], []},
      fn rewritings, {current_ucq, added, removed} ->
        {new_cover, new_added, new_removed} = rewritings
          |> most_general(current_ucq)
        {new_cover, added++new_added, removed++new_removed}
      end
    )
    new_queue = (new_queue ++ added) -- removed
    {ucq, new_queue}
  end


  def rewrite_queue([], ucq, _rules, _drules) do
    ucq
  end

  def rewrite_disj(ucq, rules, drules) do
    ### Continue here!!!!!
    ## TODO only when there are new rules.
    Logger.info("#{drules |> length} DERules")
    Logger.info("Rewriting #{ucq |> length} UCQs with #{rules |> length} Erules")
    new_cc = ucq |> Enum.flat_map(
      fn cc ->
        Logger.debug("Expand #{cc}")
        cc |> one_step_rewrite(rules)
      end
    )
    {new_cc, _, _} = new_cc |> most_general([])
    Logger.info("New CQs -> #{new_cc|> length}")
    Logger.debug("New CQs #{new_cc|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")
    {new_cover, added, removed} = new_cc
      |> most_general(ucq)
    new_cq = added
    if (added |> length) > 0 do
      Logger.info("Added CQs -> #{added |> length}")
    end
    if (removed |> length) > 0 do
      Logger.info("Removed CQs #{removed|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")
    end

    remaining = (ucq -- removed) -- added

    ucq = new_cover
    ## take the new CQs and rewrite all the disj rules
    {new_er, new_dr} = rewrite_derules(new_cq, drules, new_cq, [], [])
    ## take the new disj rules and rewrite them using all the CQs
    {new_er, new_dr} = rewrite_derules(remaining, new_dr, ucq, new_er, new_dr)
    ## this will have generated new_er and new_dr
    if (added |> length) + (new_dr |> length) + (new_er |> length) > 0 do

      if (new_er |> length) > 0 do
        Logger.info("New ERs #{new_er|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")
      end
      if (new_dr |> length) > 0 do
        Logger.info("New DERs #{new_dr|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")
      end
      rewrite_disj(ucq, rules++new_er, drules++new_dr)
    else
      ucq
    end

  end

  def rewrite_disj_queue(queue, ucq, rules, new_rules, drules) do


    # Logger.info("STEP ==> CQs #{ucq|> length}")

    ## new rules should be used in the ucq

    # {ucq, new_cq1} = rewrite_async_queue_one_step(ucq, queue, ucq, new_rules)
    # {ucq, new_cq} = rewrite_async_queue_one_step(new_cq1,[], ucq, rules++new_rules)
    {ucq, new_cq1} = rewrite_queue_one_step(ucq, queue, ucq, new_rules)
    {ucq, new_cq} = rewrite_queue_one_step(new_cq1,[], ucq, rules++new_rules)

    new_cq2 = new_cq ++ new_cq1

    ## take the new CQs and rewrite all the existing disj rules
    {new_er, new_dr} = rewrite_derules(new_cq2, drules, new_cq2, [], [])
    Logger.debug("Produced ERs #{new_er|> length}, DERs #{new_dr|> length}")
    ## take the new disj rules and rewrite them using all the CQs
    {new_er, new_dr} = rewrite_derules(ucq, new_dr, ucq, new_er, new_dr)
    ## this will have generated new_er and new_dr
    if (new_cq |> length) + (new_dr |> length) + (new_er |> length) > 0 do

      Logger.info("New ==> ERs #{new_er|> length}, DERs #{new_dr|> length}, CQs #{new_cq|> length}")

      if (new_er |> length) > 0 do
        Logger.debug("New ERs #{new_er|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")
      end
      if (new_dr |> length) > 0 do
        Logger.debug("New DERs #{new_dr|> Enum.map(&("#{&1}")) |> Enum.join(", ")}")
      end
      rewrite_disj_queue(new_cq, ucq, rules++new_rules, new_er, drules++new_dr)
    else
      ucq
    end

  end


  def rewrite_derules(_, [], _ucq, rules, drules) do
    {rules, drules}
  end

  def rewrite_derules([], [_dr|drrest], ucq, rules, drules) do
    rewrite_derules(ucq,drrest, ucq, rules, drules)
  end

  def rewrite_derules([cc|queue], [dr|drrest], ucq, rules, drules) do
    {new_er, new_dr} = cc |> one_step_drewrite(dr)
    rewrite_derules(queue, [dr|drrest] ++ new_dr, ucq, rules++new_er, drules++new_dr)
  end

  def cover(new_cc, ucq, queue) do
    {new_cover, added, removed} = new_cc
      |> most_general(ucq)
    # IO.puts "--------------------"
    # IO.puts "New Cover"
    # IO.puts new_cover |> Enum.map(&("#{&1}"))
    # IO.puts "Added"
    # IO.puts added |> Enum.map(&("#{&1}"))
    # IO.puts "Removed"
    # IO.puts removed |> Enum.map(&("#{&1}"))
    {new_cover, (queue -- removed) ++ added}
  end

  ### Do all this in parallel

  # defp comparation_groups(list, cc1) do
  #   list |> Enum.group_by(fn ccx ->
  #     two = (ccx |> subsumes(cc1))
  #     cond do
  #       two -> :more_general
  #       !(cc1 |> subsumes(ccx)) -> :different
  #       true -> :subsumed
  #     end
  #   end, fn ccx ->
  #     ccx
  #   end
  #   )
  # end

  @doc """
  tells how is clause2 related to clause1:
    :more_general if clause2 subsumes clause1
    :different if non of them subsumes each other.
    :subsumed if it is subsumed by clause1.
  """
  def compare(clause2, clause1) do
    two = (clause2 |> subsumes(clause1))
    cond do
      two -> :more_general
      !(clause1 |> subsumes(clause2)) -> :different
      true -> :subsumed
    end
  end

  @doc """
  performs in parallel the comparison of a list of clauses with respect to a clause.
  it gives a list of those that are more general, subsumed or different.
  """
  defp comparation_groups(list, cc1) do

    async_compare = fn(ccx) ->
      caller = self()
      spawn(fn ->
        res = ccx |> compare(cc1)
        send(caller, {:result, {res, ccx}})
      end)
    end

    get_result = fn ->
      receive do
      {:result, result} -> result
      end
    end

    list |> Enum.map(&async_compare.(&1))
    |> Enum.map(fn(_) -> get_result.() end)
    |> Enum.group_by(
      fn {cmp, _cc} -> cmp end,
      fn {_cmp, ccx} -> ccx  end
    )
  end


  defp comparation_groups4(list, cc1) do

    subsumed = list |> Enum.any?(fn c -> subsumes(c,cc1) end)
    if subsumed do
      %{}
    else
      async_compare = fn(ccx) ->
        caller = self()
        spawn(fn ->
          res = cc1 |> subsumes(ccx)
          send(caller, {:result, {if res do :subsumed else :different end, ccx}})
        end)
      end

      get_result = fn ->
        receive do
        {:result, result} -> result
        end
      end

      list |> Stream.map(&async_compare.(&1))
      |> Stream.map(fn(_) -> get_result.() end)
      |> Enum.group_by(
        fn {cmp, _cc} -> cmp end,
        fn {_cmp, ccx} -> ccx  end
      )
    end
  end


  defp comparation_groups1(list, cc1) do

    subsumed = list |> Task.async_stream(fn c -> subsumes(c,cc1) end)
        |> Enum.map(fn({:ok, result}) -> result end)
        |> Enum.any?(fn v -> v end)
    if subsumed do
      %{}
    else
      async_compare = fn(ccx) ->
        caller = self()
        spawn(fn ->
          res = cc1 |> subsumes(ccx)
          send(caller, {:result, {if res do :subsumed else :different end, ccx}})
        end)
      end

      get_result = fn ->
        receive do
        {:result, result} -> result
        end
      end

      list |> Stream.map(&async_compare.(&1))
      |> Stream.map(fn(_) -> get_result.() end)
      |> Enum.group_by(
        fn {cmp, _cc} -> cmp end,
        fn {_cmp, ccx} -> ccx  end
      )
    end
  end

  defp comparation_groups11(list, cc1) do

    async_compare = fn(ccx) ->
      caller = self()
      spawn(fn ->
        res = ccx |> compare(cc1)
        send(caller, {:result, {res, ccx}})
      end)
    end

    get_result = fn ->
      receive do
      {:result, result} -> result
      end
    end

    list |> Stream.map(&async_compare.(&1))
    |> Stream.map(fn(_) -> get_result.() end)
    |> Stream.take_while(fn {cmp, _cc} -> cmp != :more_general end)
    |> Enum.group_by(
      fn {cmp, _cc} -> cmp end,
      fn {_cmp, ccx} -> ccx  end
    )
  end


  defp comparation_groups3(list, cc1) do

    list |> Stream.map(fn c -> {compare(c,cc1), c} end)
    |> Stream.take_while(fn {cmp, _cc} -> cmp != :more_general end)
    |> Enum.group_by(
      fn {cmp, _cc} -> cmp end,
      fn {_cmp, ccx} -> ccx  end
    )
  end


  def most_general([], cover_old) do
    {cover_old, [], []}
  end

  # def most_general([cc1 | cc_rest], cover_old) do
  #   {cover_new, added, removed}= cc_rest |> most_general(cover_old)

  #   groups= cover_new
  #     |> Enum.group_by(fn ccx ->
  #       !(cc1 |> subsumes(ccx)) or (ccx |> subsumes(cc1))
  #     end, fn ccx ->
  #       ccx
  #     end
  #     )
  #   cover_r = Map.get(groups, true, [])
  #   rem1 = Map.get(groups, false, [])

  #   IO.inspect( groups |> Enum.map(fn {k, v} -> {k, v |> length} end))

  #   if cover_r |> length < cover_new |> length do
  #     {[cc1 | cover_r], [cc1|added], removed++rem1}
  #   else
  #     any = cover_new |> Enum.any?(fn ccx ->
  #       ccx |> subsumes(cc1)
  #     end)
  #     if any do
  #       {cover_new, added, removed}
  #     else
  #       {[cc1 | cover_new], [cc1| added], removed}
  #     end
  #   end
  # end


  # def most_general([cc1 | cc_rest], cover_old) do
  #   {cover_new, added, removed}= cc_rest |> most_general(cover_old)

  #   groups= cover_new
  #     |> comparation_groups(cc1)
  #   cover_r = Map.get(groups, :different, []) ++  Map.get(groups, :more_general, [])
  #   rem1 = Map.get(groups, :subsumed, [])

  #   # IO.inspect( groups |> Enum.map(fn {k, v} -> {k, v |> length} end))

  #   if cover_r |> length < cover_new |> length do
  #     {[cc1 | cover_r], [cc1|added]--rem1, removed++rem1}
  #   else
  #     # any = cover_new |> Enum.any?(fn ccx ->
  #     #   ccx |> subsumes(cc1)
  #     # end)
  #     if Map.get(groups, :more_general, []) |> length > 0 do
  #       {cover_new, added, removed}
  #     else
  #       {[cc1 | cover_new], [cc1| added], removed}
  #     end
  #   end
  # end



  def most_general([cc1 | cc_rest], cover_old) do
    {cover_new, added, removed}= cc_rest |> most_general(cover_old)

    groups= cover_new
      |> comparation_groups(cc1)
    cover_r = Map.get(groups, :different, []) ++  Map.get(groups, :more_general, [])
    rem1 = Map.get(groups, :subsumed, [])

    # IO.inspect( groups |> Enum.map(fn {k, v} -> {k, v |> length} end))


    if Map.get(groups, :more_general, []) |> length > 0 or length(cover_r) + length(rem1) < cover_new |> length do
      {cover_new, added, removed}
    else
      {[cc1 | cover_r], [cc1|added]--rem1, removed++rem1}
    end
  end




  @spec most_general_covers([any], any) :: {any, [any], any}
  def most_general_covers([], cover_old) do
    {cover_old, [], []}
  end

  def most_general_covers([cc1 | cc_rest], cover_old) do
    {cover_new, added, removed}= cc_rest |> most_general_covers(cover_old)

    groups= (cover_new--added)
      |>comparation_groups(cc1)
    cover_r = Map.get(groups, :different, []) ++  Map.get(groups, :more_general, [])
    rem1 = Map.get(groups, :subsumed, [])


    # IO.puts "not subsumed"
    # IO.puts cover_r |> Enum.map(&("#{&1}"))
    # IO.puts "subsumed"
    # IO.puts rem1 |> Enum.map(&("#{&1}"))

    if cover_r |> length < cover_new |> length do
      {[cc1 | cover_r], [cc1|added]--rem1, removed++rem1}
    else
      # any = cover_new |> Enum.any?(fn ccx ->
      #   ccx |> subsumes(cc1)
      # end)
      if Map.get(groups, :more_general, []) |> length > 0  do
        {cover_new, added, removed}
      else
        {[cc1 | cover_new], [cc1| added], removed}
      end
    end
  end

end
