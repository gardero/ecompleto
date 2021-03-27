# ECompleto

## Running

From bash:
```
time mix run -e "ECompleto.Experiments.rewrite('ontologies/travel.dlgp', 'ontologies/travel.queries2.txt') |> Enum.map(&(\"#{&1}\"))"  
```

From `iex -S mix`
```
iex(1)> ECompleto.Experiments.rewrite('ontologies/travel.dlgp', 'ontologies/travel.queries.txt', 0)
```

** TODO 
Achieve this under 2 seconds
```
ECompleto.Experiments.rewrite('ontologies/travel.dlgp', 'ontologies/travel.queries2.txt')
```
Currenly it takes around 10 seconds.


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

The `DLGP+` is an extension of the existing [`DLGP` v2.0](https://graphik-team.github.io/graal/papers/datalog+_v2.0_en.pdf) notation that allows the especification of disjunctive existential rules and negated atoms in queries.

Disjunction is specified in the head of a rule by writting a list in squared brackets. The disjoint elements can be a sigle atom or a serveral atoms enclosed in brackets, e.g.,
```
[disj] [leaf(X), (inner_node(X), edge(X,Y))] :- node(X).

```
Negation in queries with negated atoms is sprecified with the minus symbol, e.g.,
```
[q] ? :- person(X), -marriedTo(X,Y). 
```

## Notes



1. Consider that currently Skolem functions use the variable name as the functor of the Skolem term. This is ok if we do rewriting with respect to constraint clauses and never propagate those Skolem terms. However, the Chase algorithms need to ensure that the nulls generated have a globally unique ID.
1. Implement some sort of substitution for the prefixes in the OWL notation inside the `dlgp` programs.
