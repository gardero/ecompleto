# ECompleto

ECompleto is a query rewriting system that supports queries with universally quantified negation and ontologies with disjunctive existential rules. It Provides a UCQ rewriting that can be used to find all the answers of the input query with respect to the rules in the ontology.

## Running

From bash:
```bash
time mix run -e "ECompleto.Experiments.rewrite('ontologies/travel.dlgp', 'ontologies/travel.queries2.txt') |> Enum.map(&(\"#{&1}\"))"  
```

From `iex -S mix`
```elixir
iex(1)> ECompleto.Experiments.rewrite('ontologies/travel.dlgp', 'ontologies/travel.queries.txt', 0)
```

** TODO 
Achieve this in under 2 seconds
```elixir
ECompleto.Experiments.rewrite('ontologies/travel.dlgp', 'ontologies/travel.queries2.txt')
```
Currently, it takes around 10 seconds.

## Usage Examples

* To get the rewritings of a query
```elixir
ECompleto.Experiments.rewrite('experiments/AGOSUV-bench/A/A.dlp', 'experiments/AGOSUV-bench/A/A_queries.dlp', 0) |>  ECompleto.Program.to_file('experiments/AGOSUV-bench/A/A_rewritings_0_ecompleto.dlgp')
```

* To get the answers of a query
```elixir
ECompleto.Experiments.answer('experiments/AGOSUV-bench/A/A.dlp', 'experiments/AGOSUV-bench/A/A_queries.dlp', 0) |>  ECompleto.Program.to_file('experiments/AGOSUV-bench/A/A_answers_0_ecompleto.dlgp')
```


## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `ecompleto` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ecompleto, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/ecompleto](https://hexdocs.pm/ecompleto).

## DLGP+ Notation

The `DLGP+` is an extension of the existing [`DLGP` v2.0](https://graphik-team.github.io/graal/papers/datalog+_v2.0_en.pdf) notation that allows the specification of disjunctive existential rules and negated atoms in queries.

Disjunction is specified in the head of a rule by writing a list in squared brackets. The disjoint elements can be a single atom or several atoms enclosed in brackets, e.g.,
```
[disj] [leaf(X), (inner_node(X), edge(X,Y))] :- node(X).

```
Negation in queries with negated atoms is specified with the minus symbol, e.g.,
```
[q] ? :- person(X), -marriedTo(X,Y). 
```

## Notes



1. Consider that currently Skolem functions use the variable name as the functor of the Skolem term. This is ok if we do rewriting with respect to constraint clauses and never propagate those Skolem terms. However, the Chase algorithms need to ensure that the nulls generated have a globally unique ID.
1. Implement some sort of substitution for the prefixes in the OWL notation inside the `dlgp` programs.

## Bugs

### Queries with one negated atom (Fixed)
There is a problem when the initial query has only one negated atom. It is incorrectly seen as a disjunctive rule and then the queries produced remain as disjunctive rules and do not act like queries.

For the initial query `[query2] ? (Y) :- :Object(Y), -:Blue(Y).` we have:

```
13:15:22.417 [debug] Rewrite DER ! :- <http://sw.islab.ntua.gr/xai/CLEVR-Hans3#Yellow>(X0_1), answer_atom(X0_1), <http://sw.islab.ntua.gr/xai/CLEVR-Hans3#Object>(X0_1).                                                                                                                                                                                                                                                              13:15:22.417 [debug] Using CC [-<http://sw.islab.ntua.gr/xai/CLEVR-Hans3#Gray>(X0_1), -<http://sw.islab.ntua.gr/xai/CLEVR-Hans3#Yellow>(X0_1)]                                                                                                                                                                                                                                                                                        13:15:22.417 [debug] Rewrite DER ! :- <http://sw.islab.ntua.gr/xai/CLEVR-Hans3#Yellow>(X0_1), answer_atom(X0_1), <http://sw.islab.ntua.gr/xai/CLEVR-Hans3#Object>(X0_1).                                                                                                                                                                                                                                                              13:15:22.419 [debug] Using CC [-<http://sw.islab.ntua.gr/xai/CLEVR-Hans3#Green>(X0_1), -<http://sw.islab.ntua.gr/xai/CLEVR-Hans3#Red>(X0_1)]                                                                                             
```
```
/usr/bin/time -f "%e, %M" --output=../../results/lubm2.queries.txt.output.q280+.txt time mix run -e "ECompleto.Experiments.rewrite('../../ontologies/lubm.dlgp', '../../ontologies/lubm2.queries.txt',280) |> ECompleto.Program.to_file('../../results/lubm2.queries.txt.rewritings.q288+.txt')"
```
