%{
      open Execute
%}

(* expr *)
%token <string> STRING INT BOOL IDENTIFIER
%token L_PARA R_PARA ADD SUB MUL DIV MOD EXP AND OR NOT EQ LT LTE GT GTE NEQ

(* stmt *)
%token EOL EOF
%token ASSIGN
%token IF THEN ELSE END_IF CASE OF END_CASE
%token FOR TO BY DO END_FOR WHILE END_WHILE REPEAT UNTIL END_REPEAT


(* precedences *)
%left OR
%left AND
%left XOR
%left EQ NEQ
%left LT LTE GT GTE
%left ADD SUB
%left MUL DIV MOD
%right NOT
%nonassoc EXP

%start main
%type <Execute.stmt> main
%type <Execute.expr> expr
%type <Execute.stmt list> stmt_list
%type <Execute.stmt> stmt
%%
main:
      stmt_list EOF  {Seq $1}
;
expr:
      INT                   { Const (Int (int_of_string $1)) }
    | BOOL                  { Const (Bool (bool_of_string $1)) }
    | STRING                { Const (String $1); }
    | L_PARA expr R_PARA    { $2 }
    | IDENTIFIER            { Var $1 }
    | expr ADD expr         { Add ($1, $3) }
    | expr SUB expr         { Sub ($1, $3) }
    | expr MUL expr         { Mul ($1, $3) }
    | expr DIV expr         { Div ($1, $3) }
    | expr MOD expr         { Mod ($1, $3) }
    | expr EXP expr         { Exp ($1, $3) }
    | expr AND expr         { And ($1, $3) }
    | expr OR expr          { Or ($1, $3) }
    | expr LT expr          { Lt0 ($1, $3) }
    | expr LTE expr         { Lte ($1, $3) }
    | expr GT expr          { Gt0 ($1, $3) }
    | expr GTE expr         { Gte ($1, $3) }
    | expr EQ expr          { Eq0 ($1, $3) }
    | expr NEQ expr         { Neq ($1, $3) }
;
stmt:
      EOL                                                                     { Skip }
    | IDENTIFIER ASSIGN expr EOL                                              { Assign ($1, $3) }
    | IF expr THEN stmt_list ELSE stmt_list END_IF EOL                        { If ($2, (Seq $4), (Seq $6)) }
    | CASE expr OF stmt_list END_CASE EOL                                     { Case ($2, $4) }
    | FOR IDENTIFIER ASSIGN expr TO expr BY expr DO stmt_list END_FOR EOL     { Skip }
    | WHILE expr DO stmt_list END_WHILE EOL                                   { While ($2, (Seq $4)) }
    | REPEAT stmt_list UNTIL expr END_REPEAT EOL                              { Seq [ While ($4, (Seq $2))] }
;
stmt_list:
        stmt stmt_list  { $1 :: $2 }
      | stmt            { [$1] }
;
