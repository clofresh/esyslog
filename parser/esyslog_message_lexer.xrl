Definitions.

WHITESPACE = [\t\s]
PRIORITY = [1-9]+[0-9]*
MONTH = [A-Za-z][A-Za-z][A-Za-z]
DAY = ([1-3][0-9]|\s[1-9])
HOUR = ([0-2][0-9]|\s[1-9])
MINUTE = [0-5][0-9]
SECOND = [0-5][0-9]

Rules.

<{PRIORITY}> : {token, {priority, priority(TokenChars)}}.
{MONTH}\s{DAY}\s{HOUR}\:{MINUTE}\:{SECOND} : {token, {datetime, TokenChars}}.

{WHITESPACE}+ : skip_token.
[^\:\s]+ : {token, {word, TokenChars}}.
\: : {token, {':', TokenChars}}.

Erlang code.

priority(Token) ->
    list_to_integer(
        lists:filter(
            fun
                ($<) -> false;
                ($>) -> false;
                (_)   -> true
            end,
            Token
        )
    ).
    
