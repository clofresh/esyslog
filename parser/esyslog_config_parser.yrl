Nonterminals 
actions action facility severity target newlines.

Terminals
word filepath host port wildcard newline '.' ':'.

Rootsymbol actions.

actions -> action : ['$1'].
actions -> action newlines : ['$1'].
actions -> action newlines actions : lists:append(['$1'], '$3').
action -> facility '.' severity target : [{facility, '$1'}, 
                                          {severity, '$3'}, 
                                          {target,   '$4'}].

facility -> word     : '$1'.
facility -> wildcard : '$1'.

severity -> word     : '$1'.
severity -> wildcard : '$1'.

target -> filepath            : {local,  '$1'}.
target -> host            : {remote, '$1'}.
target -> host ':' port : {remote, {'$1', '$3'}}.

newlines -> newline : nothin.
newlines -> newline newlines : nothin.
