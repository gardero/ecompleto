
Definitions.


COMMENT = \%[^\n\r]*

UPPERCASE_LETTER = [A-Z]
LOWERCASE_LETTER = [a-z]
LETTER = {UPPERCASE_LETTER}|{LOWERCASE_LETTER}
DIGIT = [0-9]
SIMPLE_CHAR = {LETTER}|{DIGIT}|[\_]
CHAR = [^\n\\\"]
SIGN = [\+\-]
EXPO = [eE]
SIMPLE_STRING = {SIMPLE_CHAR}+
NORMAL_STRING = {CHAR}+
NUMBER = {DIGIT}+
EXPONENT = {EXPO}{SIGN}{NUMBER}
WHITESPACE = [\s\t\n\r]

FLOAT = [\-]{NUMBER}[\.]{DIGIT}*{EXPONENT}?|[\-][\.]{NUMBER}{EXPONENT}?|[\-]{NUMBER}{EXPONENT}

EQUALITY_MARK = \=
END_MARK = .
ABSURD_MARK = !
RULE_MARK = :-
QUESTION_MARK = \?
START_STATEMENT_MARK = \[
END_STATEMENT_MARK = \]
START_TERMS_MARK = \(
END_TERMS_MARK = \)
SEPARATOR = \,

U_IDENT = {UPPERCASE_LETTER}({SIMPLE_CHAR})*
L_IDENT = {LOWERCASE_LETTER}({SIMPLE_CHAR})*

HEX           = ([0-9]|[A-F]|[a-f])
UCHAR         = (\\u({HEX})({HEX})({HEX})({HEX}))|(\\U({HEX})({HEX})({HEX})({HEX})({HEX})({HEX})({HEX})({HEX}))
ECHAR         = \\[tbnrf"'\\]
PERCENT	      =	(%{HEX}{HEX})
PN_CHARS_BASE = ([A-Z]|[a-z]|[\x{00C0}-\x{00D6}]|[\x{00D8}-\x{00F6}]|[\x{00F8}-\x{02FF}]|[\x{0370}-\x{037D}]|[\x{037F}-\x{1FFF}]|[\x{200C}-\x{200D}]|[\x{2070}-\x{218F}]|[\x{2C00}-\x{2FEF}]|[\x{3001}-\x{D7FF}]|[\x{F900}-\x{FDCF}]|[\x{FDF0}-\x{FFFD}]|[\x{10000}-\x{EFFFF}])
PN_CHARS_U    = ({PN_CHARS_BASE}|_)
PN_CHARS      = ({PN_CHARS_U}|-|[0-9]|\x{00B7}|[\x{0300}-\x{036F}]|[\x{203F}-\x{2040}])
PN_PREFIX	    =	({PN_CHARS_BASE}(({PN_CHARS}|\.)*{PN_CHARS})?)
PN_LOCAL_ESC  =	\\(_|~|\.|\-|\!|\$|\&|\'|\(|\)|\*|\+|\,|\;|\=|\/|\?|\#|\@|\%)
PLX           =	({PERCENT}|{PN_LOCAL_ESC})
PN_LOCAL	    =	({PN_CHARS_U}|:|[0-9]|{PLX})(({PN_CHARS}|\.|:|{PLX})*({PN_CHARS}|:|{PLX}))?
PNAME_NS	    =	{PN_PREFIX}?:
PNAME_LN	    =	{PNAME_NS}{PN_LOCAL}
EXPONENT	=	([eE][+-]?[0-9]+)
BOOLEAN   = true|false
INTEGER	  =	[+-]?[0-9]+
DECIMAL	  =	[+-]?[0-9]*\.[0-9]+
DOUBLE	  =	[+-]?([0-9]+\.[0-9]*{EXPONENT}|\.[0-9]+{EXPONENT}|[0-9]+{EXPONENT})
IRIREF = <([^\x00-\x20<>"{}|^`\\]|{UCHAR})*>
STRING_LITERAL_QUOTE              = "([^"\\\n\r]|{ECHAR}|{UCHAR})*"
STRING_LITERAL_SINGLE_QUOTE	      =	'([^'\\\n\r]|{ECHAR}|{UCHAR})*'
STRING_LITERAL_LONG_SINGLE_QUOTE	=	'''(('|'')?([^'\\]|{ECHAR}|{UCHAR}))*'''
STRING_LITERAL_LONG_QUOTE	        =	"""(("|"")?([^"\\]|{ECHAR}|{UCHAR}))*"""
BLANK_NODE_LABEL = _:({PN_CHARS_U}|[0-9])(({PN_CHARS}|\.)*({PN_CHARS}))?
LANGTAG	=	@[a-zA-Z]+(-[a-zA-Z0-9]+)*
BASE    = [Bb][Aa][Ss][Ee]
PREFIX  = [Pp][Rr][Ee][Ff][Ii][Xx]

PrefixedName = {PNAME_LN}|{PNAME_NS}

Rules.


@rules            : {token, {'@rules',  TokenLine}}.
@facts            : {token, {'@facts',  TokenLine}}.
@constraints            : {token, {'@constraints',  TokenLine}}.
@queries            : {token, {'@queries',  TokenLine}}.
@base            : {token, {'@base',  TokenLine}}.
@prefix            : {token, {'@prefix',  TokenLine}}.
@top            : {token, {'@top',  TokenLine}}.
@una            : {token, {'@una',  TokenLine}}.


\^\^            : {token, {'^^',  TokenLine}}.

\-            : {token, {negation,  TokenLine}}.
\=            : {token, {'=',  TokenLine}}.
\.            : {token, {'.',  TokenLine}}.
,             : {token, {',',  TokenLine}}.
\[            : {token, {'[',  TokenLine}}.
\]            : {token, {']',  TokenLine}}.
\!             : {token, {'!',  TokenLine}}.
\(            : {token, {'(',  TokenLine}}.
\)            : {token, {')',  TokenLine}}.
\?             : {token, {'?',  TokenLine}}.
\:\-            : {token, {':-',  TokenLine}}.



{IRIREF}                           : {token, {iriref, TokenLine,  quoted_content_str(TokenChars)}}.
{LANGTAG}                          : {token, {langtag, TokenLine, langtag_str(TokenChars)}}.
{IRIREF}                           : {token, {iriref,  TokenLine, quoted_content_str(TokenChars)}}.
{DOUBLE}                           : {token, {double, TokenLine, double(TokenChars)}}.
{DECIMAL}                          : {token, {decimal, TokenLine, decimal(TokenChars)}}.
{INTEGER}	                         : {token, {integer,  TokenLine, integer(TokenChars)}}.
{BOOLEAN}                          : {token, {boolean, TokenLine, boolean(TokenChars)}}.
{STRING_LITERAL_SINGLE_QUOTE}      : {token, {string_literal_quote, TokenLine, quoted_content_str(TokenChars)}}.
{STRING_LITERAL_QUOTE}             : {token, {string_literal_quote, TokenLine, quoted_content_str(TokenChars)}}.
{STRING_LITERAL_LONG_SINGLE_QUOTE} : {token, {string_literal_quote, TokenLine, long_quoted_content_str(TokenChars)}}.
{STRING_LITERAL_LONG_QUOTE}        : {token, {string_literal_quote, TokenLine, long_quoted_content_str(TokenChars)}}.

{PNAME_NS}                         : {token, {prefix_ns, TokenLine, prefix_ns(TokenChars)}}.
{PNAME_LN}                         : {token, {prefix_ln, TokenLine, prefix_ln(TokenChars)}}.

{U_IDENT} : {token, {u_ident, TokenLine, TokenChars}}.
{L_IDENT} : {token, {l_ident, TokenLine, TokenChars}}.

\"{NORMAL_STRING}\"               : {token, {double_quoted_string, TokenLine, string:trim(TokenChars, both, "\"")}}.

0|[\-]?[1-9]{DIGIT}*             : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

{FLOAT} : {token, {float, TokenLine, TokenChars}}.



{WHITESPACE}+ : skip_token.
{COMMENT} : skip_token.

Erlang code.



% integer(TokenChars)  -> TokenChars.
% decimal(TokenChars)  -> TokenChars.
% double(TokenChars)   -> TokenChars.
% boolean(TokenChars)  -> TokenChars.
% quoted_content_str(TokenChars) -> TokenChars.
% long_quoted_content_str(TokenChars) -> TokenChars.
% bnode_str(TokenChars) -> TokenChars.
% langtag_str(TokenChars) -> TokenChars.
% prefix_ns(TokenChars) -> TokenChars.
% prefix_ln(TokenChars) -> TokenChars.


integer(TokenChars)  -> 'Elixir.RDF.Serialization.ParseHelper':integer(TokenChars).
decimal(TokenChars)  -> 'Elixir.RDF.Serialization.ParseHelper':decimal(TokenChars).
double(TokenChars)   -> 'Elixir.RDF.Serialization.ParseHelper':double(TokenChars).
boolean(TokenChars)  -> 'Elixir.RDF.Serialization.ParseHelper':boolean(TokenChars).
quoted_content_str(TokenChars) -> 'Elixir.RDF.Serialization.ParseHelper':quoted_content_str(TokenChars).
long_quoted_content_str(TokenChars) -> 'Elixir.RDF.Serialization.ParseHelper':long_quoted_content_str(TokenChars).
bnode_str(TokenChars) -> 'Elixir.RDF.Serialization.ParseHelper':bnode_str(TokenChars).
langtag_str(TokenChars) -> 'Elixir.RDF.Serialization.ParseHelper':langtag_str(TokenChars).
prefix_ns(TokenChars) -> 'Elixir.RDF.Serialization.ParseHelper':prefix_ns(TokenChars).
prefix_ln(TokenChars) -> 'Elixir.RDF.Serialization.ParseHelper':prefix_ln(TokenChars).