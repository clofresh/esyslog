Definitions.

WHITESPACE = [\t\s]
CHAR = [A-Za-z0-9_-]
FILEPATH_CHAR = [A-Za-z0-9_\-/]
IP = ([0-9]|[1-9][0-9]|[1-9][0-9][0-9])
D = [0-9]

Rules.

@{IP}\.{IP}\.{IP}\.{IP} : {token,{host,TokenLine,list_to_atom(tl(TokenChars))}}.
@[A-Za-z]({CHAR}|\.)* : {token,{host,TokenLine,list_to_atom(tl(TokenChars))}}.
{D}+ : {token,{port,TokenLine,list_to_integer(TokenChars)}}.
[A-Za-z]{CHAR}* : {token,{word,TokenLine,list_to_atom(TokenChars)}}.
{FILEPATH_CHAR}+ : {token,{filepath,TokenLine,list_to_atom(TokenChars)}}.

\. : {token, {'.', TokenLine}}.
\: : {token, {':', TokenLine}}.
\* : {token, {wildcard, TokenLine}}.
\n : {token, {newline, TokenLine}}.


{WHITESPACE}+ : skip_token.

Erlang code.

