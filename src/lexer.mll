{
    open Parser
}

rule token = parse 
      [' ' '\t' '\n']             {token lexbuf} (* Skip blanks *)
    | ';' | ";\n"                 {EOL}
    | eof                         {EOF}

    | '-'?['0' - '9']+ as lxm     {INT lxm}
    | "true" | "false" as lxm     {BOOL lxm}
    | '"'_*'"' as lxm             {STRING lxm}
    | '('                         {L_PARA}
    | ')'                         {R_PARA}
    | '+'                         {ADD}
    | '-'                         {SUB}
    | '*'                         {MUL}
    | '/'                         {DIV}
    | '%'                         {MOD}
    | '^'                         {EXP}
    | '&' | "&&" | "and"          {AND}
    | '|' | "||" | "or"           {OR}
    | '!' | "not"                 {NOT}
    | '=' | "=="                  {EQ}
    | '<'                         {LT}
    | "<="                        {LTE}
    | '>'                         {GT}
    | ">="                        {GTE}
    | "!="                        {NEQ}

    | ":="                        {ASSIGN}
    | "IF"                        {IF}
    | "THEN"                      {THEN}
    | "ELSE"                      {ELSE}
    | "END_IF" | "ENDIF"          {END_IF}
    | "CASE"                      {CASE}
    | "OF"                        {OF}
    | "ENDCASE" | "END_CASE"      {END_CASE}
    | "FOR"                       {FOR}
    | "TO"                        {TO}
    | "BY"                        {BY}
    | "DO"                        {DO}
    | "ENDFOR" | "END_FOR"        {END_FOR}
    | "WHILE"                     {WHILE}
    | "ENDWHILE" | "END_WHILE"    {END_WHILE}
    | "REPEAT"                    {REPEAT}
    | "UNTIL"                     {UNTIL}
    | "ENDREPEAT" | "END_REPEAT"  {END_REPEAT}

    | ['a'-'z']['a'-'z' '0'-'9']* as lxm                 {IDENTIFIER lxm}