

Nonterminals document statement_list statement fact drule rule constraint cquery directive_list annotation
header header_one r_body r_head
identifier body head atom equality std_atom term_list conjunction statement_expr iri prefixedName nquery conjunction_csf conjunction_csf_list dr_head
rdfLiteral numericLiteral booleanLiteral label label_one section section_list conjunction_atoms conjunction_neg neg_literal
term var_list predicate variable constant literal.
Terminals '[' '(' ')' ']' ',' '^^' integer double decimal boolean langtag string_literal_quote double_quoted_string u_ident l_ident '.' ':-' '!' '?' '=' negation iriref 
  '@facts' '@rules'  '@constraints' '@queries' '@base' '@prefix' '@top' prefix_ln prefix_ns
.
Rootsymbol document.


document -> header body : new_program('$1', '$2').
document -> body : new_program([],'$1').

header_one -> '@base' iriref : {base, to_iri_string('$2')}.
header_one -> '@prefix' prefix_ns iriref : {extract_token('$2'), to_iri_string('$3')}.
header_one -> '@top' l_ident : {top, extract_token('$2')}.
header_one -> '@top' iriref : {top, extract_token('$2')}.

header -> header_one : put_tuple('$1').
header -> header_one header : put_tuple('$1','$2').

body -> section_list : '$1'.
body -> statement_list : '$1'.

% directive_list -> statement_list : ['$1'].
% directive_list -> annotation : ['$1'].
% directive_list -> statement directive_list : ['$1' | '$2'].
% directive_list -> annotation directive_list : ['$1' | '$2'].

annotation -> '@facts' : '$1'.
annotation -> '@rules' : '$1'.
annotation -> '@constraints' : '$1'.
annotation -> '@queries' : '$1'.

section -> annotation : [].
section -> annotation statement_list : '$2'.

section_list -> section : '$1'.
section -> section section_list : '$1'++'$2'.


statement_list -> statement : ['$1'].
statement_list -> statement statement_list : ['$1'|'$2'].

statement -> statement_expr : '$1'.
statement -> '[' label ']' statement_expr : put_alias('$4','$2').


label_one -> l_ident : '$1'.
label_one -> integer : '$1'.
label -> label_one : [extract_token('$1')].
label -> label_one label : [extract_token('$1')| '$2'].

statement_expr -> fact : '$1'.
statement_expr -> rule : '$1'.
statement_expr -> drule : '$1'.
statement_expr -> constraint : '$1'.
statement_expr -> cquery : '$1'.
statement_expr -> nquery : '$1'.

fact -> conjunction_atoms '.' : new_fact('$1').


rule -> r_head ':-' r_body '.' : new_erule('$1', '$3').
rule -> r_head ':-' '.' : new_erule('$1', []).

r_head -> conjunction_atoms : '$1'.
r_body -> conjunction_atoms : '$1'.

drule -> dr_head ':-' r_body '.' : new_derule('$1', '$3').
drule -> dr_head ':-' '.' : new_derule('$1', []).

dr_head -> '[' conjunction_csf_list ']' : '$2'.

conjunction_csf -> atom : ['$1'].
conjunction_csf -> '(' conjunction_atoms ')' : '$2'.

conjunction_csf_list -> conjunction_csf : ['$1'].
conjunction_csf_list -> conjunction_csf ',' conjunction_csf_list: ['$1' | '$3'].

constraint -> '!'  ':-' conjunction_atoms '.' : new_constraint('$3').

cquery -> '?' '(' var_list ')' ':-' conjunction_atoms '.' : new_cquery('$3', '$6').
cquery -> '?' ':-' conjunction_atoms '.' : new_cquery([], '$3').
cquery -> '?' ':-' '.' : new_cquery([], []).

nquery -> '?' '(' var_list ')' ':-' conjunction_neg '.' : new_nquery('$3', '$6').
nquery -> '?' ':-' conjunction_neg '.' : new_nquery([], '$3').

conjunction_neg -> neg_literal : ['$1'].
conjunction_neg -> neg_literal ',' conjunction_atoms: ['$1' | '$3'].
conjunction_neg -> atom ',' conjunction_neg : ['$1' | '$3'].
conjunction_neg -> neg_literal ',' conjunction_neg : ['$1' | '$3'].

conjunction_atoms -> atom : ['$1'].
conjunction_atoms -> atom ',' conjunction_atoms : ['$1' | '$3'].


neg_literal -> negation std_atom : new_complement('$2').

atom -> std_atom : '$1'.
atom -> equality : '$1'.

equality -> term '=' term : new_equality({'$1', '$3'}).
std_atom -> predicate '(' term_list ')' : new_atom('$1', '$3').

term_list -> term : ['$1'].
term_list -> term ',' term_list : ['$1' | '$3'].

term -> variable : '$1'.
term -> constant : new_term('$1',[]).


var_list -> variable : ['$1'].
var_list -> variable ',' var_list : ['$1' | '$3'].

predicate -> l_ident : extract_token('$1').
predicate -> iriref : extract_token('$1').
predicate -> prefixedName : extract_token('$1').


variable -> u_ident : new_var(extract_token('$1')).

constant -> l_ident : extract_token('$1').
constant -> iriref : extract_token('$1').
constant -> prefixedName : extract_token('$1').
constant -> literal : '$1'.



identifier -> u_ident : extract_token('$1').

identifier -> l_ident : extract_token('$1').

prefixedName -> prefix_ln : '$1' .
prefixedName -> prefix_ns : '$1' .

literal -> rdfLiteral     : '$1' .
literal -> numericLiteral : '$1' .
literal -> booleanLiteral : '$1' .


rdfLiteral -> string_literal_quote '^^' iri : to_literal(extract_token('$1'), {datatype, '$3'}) .
rdfLiteral -> string_literal_quote langtag     : to_literal(extract_token('$1'), {language, to_langtag('$2')}) .
rdfLiteral -> string_literal_quote '@prefix'   : to_literal(extract_token('$1'), {language, to_langtag('$2')}) .
rdfLiteral -> string_literal_quote '@base'     : to_literal(extract_token('$1'), {language, to_langtag('$2')}) .
rdfLiteral -> string_literal_quote             : to_literal(extract_token('$1')) .
numericLiteral -> integer : to_literal('$1') .
numericLiteral -> decimal : to_literal('$1') .
numericLiteral -> double  : to_literal('$1') .
booleanLiteral -> boolean : to_literal('$1') .

iri -> iriref       : to_iri('$1') .
iri -> prefixedName : extract_token('$1') .


Erlang code.

extract_token({_Token, _Line, Value}) -> Value.

put_tuple({K, V}) -> maps:put(K, V, #{}).
put_tuple({K, V}, Other) -> maps:put(K, V, Other).
put_new({K,V}, M) -> 'Elixir.ECompleto.Utils':put_new(M, K, V).


new_var(Name) -> 'Elixir.ECompleto.Terms':new_var(Name).
new_term(Funtor,Arguments) -> 'Elixir.ECompleto.Terms':new_term(Funtor,Arguments).
new_atom(Funtor,Arguments) -> 'Elixir.ECompleto.Clauses':new_literal(Funtor,Arguments).
new_complement(Atom) -> 'Elixir.ECompleto.Clauses':complement(Atom).
new_equality({A,B}) -> 'Elixir.ECompleto.Clauses':new_literal("=",[A,B]).
new_cquery(Answer,Body) -> 'Elixir.ECompleto.Queries':new_cquery(Answer, Body). 
new_nquery(Answer,Body) -> 'Elixir.ECompleto.Queries':new_nquery(Answer, Body). 
put_alias(E,Alias) -> put_new({alias, Alias},E).
new_erule(Head,Body) -> 'Elixir.ECompleto.Rules':new_erule(Head,Body). 
new_derule(Head,Body) -> 'Elixir.ECompleto.Rules':new_derule(Head,Body). 
new_constraint(Body) -> 'Elixir.ECompleto.Rules':new_erule([],Body). 
new_fact(Body) -> 'Elixir.ECompleto.Rules':new_erule(Body,[]). 

% new_header(Type, Body) -> new_term(Type, [Body]). 
% new_prefix(Name, Body) -> put_tuple({Name, Body}). 

new_program(Headers, Body) -> 'Elixir.ECompleto.Program':new_program(Headers,Body). 


% to_literal(STRING_LITERAL_QUOTE) -> extract_token(STRING_LITERAL_QUOTE).
% to_langtag(LANGTAG) -> LANGTAG.
% to_literal(STRING_LITERAL_QUOTE, Type) -> {extract_token(STRING_LITERAL_QUOTE), Type}.


to_iri_string(IRIREF) -> 'Elixir.RDF.Serialization.ParseHelper':to_iri_string(IRIREF) .
to_iri(IRIREF) -> 'Elixir.RDF.Serialization.ParseHelper':to_absolute_or_relative_iri(IRIREF) .
%to_bnode(BLANK_NODE) -> 'Elixir.RDF.Serialization.ParseHelper':to_bnode(BLANK_NODE).
to_literal(STRING_LITERAL_QUOTE) -> 'Elixir.RDF.Serialization.ParseHelper':to_literal(STRING_LITERAL_QUOTE).
to_literal(STRING_LITERAL_QUOTE, Type) -> 'Elixir.RDF.Serialization.ParseHelper':to_literal(STRING_LITERAL_QUOTE, Type).
to_langtag(LANGTAG) -> 'Elixir.RDF.Serialization.ParseHelper':to_langtag(LANGTAG).
%rdf_type() -> 'Elixir.RDF.Serialization.ParseHelper':rdf_type().