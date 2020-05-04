%{
open Syntax
%}

// リテラル
%token <string> ID  // x, y, abc, ...
%token <int> CONST     // 0, 1, 2, ...

// 演算子
%token PLUS     // '+'
%token MINUS    // '-'
%token CARET	// '^'
%token ASTERISK // '*'
%token SLASH    // '/'
%token PAR      // '%'
%token AND      // '&'
%token PIPE     // '|'
%token ANDAND   // "&&"
%token PIPEPIPE // "||"
%token EQUAL    // '='
%token LESS     // '<'
%token GREATER  // '>'
%token BELOW    // "<="
%token ABOVE	// ">="
%token NOT	// "!="
%token SWAP     // "<=>"
%token COMMA    // ','
%token WCOLON   // "::"
// 括弧
%token LBRA     // '['
%token RBRA     // ']'
%token LPAREN   // '('
%token RPAREN   // ')'
// キーワード
%token CLASS    // "class"
%token INHERITS // "inherits"
%token METHOD   // "method"
%token IF	// "if"
%token THEN	// "then"
%token ELSE	// "else"
%token FI	// "fi"
%token FROM	// "from"
%token DO	// "do"
%token LOOP	// "loop"
%token UNTIL    // "until"
%token CONSTRUCT // "construct"
%token DESTRUCT // "destruct"
%token CALL     // "call"
%token UNCALL   // "uncall"
%token SKIP 	// "skip"
%token NILL     // "nill"
%token INT      // "int"
// 制御記号
%token SEMI // ';'

%start main
%type <Syntax.prog> main

%%

// 開始記号
main:
  | prog SEMI
    {Cl $1 }
;

// 変数
var:
  | ID
    { Var $1 }
;

// 式
exp:
  // 定数
  | CONST
    { Const $1 }

  // 変数
  | var
    { EVar $1 }

  // nill
  | NILL
    { Nill }

  // e1 +　e2
  | exp PLUS exp
    { Plus($1, $3) }

  // e1 - e2
  | exp MINUS exp
    { Minus($1, $3) }

  // e1 ^ e2
  | exp CARET exp
    { Xor($1, $3) }

  // e1 * e2
  | exp ASTERISK exp
    { Times($1, $3) }
  
  // e1 / e2
  | exp SLASH exp
    { Div($1, $3) }

  // e1 % e2
  | exp PAR exp
    { Mod($1, $3) }

  // e1 & e2
  | exp AND exp
    { OpAnd($1, $3) }

  // e1 | e2
  | exp PIPE exp
    { OpOr($1, $3) }

  // e1 && e2
  | exp ANDAND exp
    { And($1, $3) }

  // e1 && e2
  | exp PIPEPIPE exp
    { Or($1, $3) }

  // e1 < e2
  | exp LESS exp
    { Less($1, $3) }
    
  // e1 > e2
  | exp GREATER exp
    { Greater($1, $3) }

  // e1 = e2
  | exp EQUAL exp
    { Equal($1, $3) }
  
  // e1 != e2
  | exp NOT exp
    { NotEq($1, $3) }

  // e1 <= e2
  | exp BELOW exp
    { LessEq($1, $3) } 

  // e1 >= e2
  | exp ABOVE exp
    { GreaterEq($1, $3) }

var_list:
  | var COMMA var_list
    { [$1] @ $3 }
  | var
    { [$1] }
  |
    { [] }

// statement
statement:
  // x += e 
  | var PLUS EQUAL exp
    { AssignPl($1, $4) }

  // x -= e
  | var MINUS EQUAL exp
    { AssignMin($1, $4) }

  // x ^= e
    | var CARET EQUAL exp
    { AssignXor($1, $4) }

  // x <=> x
  | var SWAP var
    { Swap($1, $3) }

  // if e then s else s fi e
  | IF exp THEN statement ELSE statement FI exp
    { If($2, $4, $6, $8) }

  // from e do s loop s until e
  | FROM exp DO statement LOOP statement UNTIL exp
    { From($2, $4, $6, $8) }

  // construct c x  s  destruct x
  | CONSTRUCT ID var statement DESTRUCT var
    { Construct(ClassId($2), $3, $4, $6) }

  // call q(x, ... , x)
  | CALL ID LPAREN var_list RPAREN 
    { LocalCall(MethId($2), $4) }

  // uncall q(x, ... , x)
  | UNCALL ID LPAREN var_list RPAREN
    { LocalUncall(MethId($2), $4) }

  // call x::q(x, ..., x)
  | CALL var WCOLON ID LPAREN var_list RPAREN
    { Call($2, MethId($4), $6) }


  // uncall x::q(x, ..., x)
  | UNCALL var WCOLON ID LPAREN var_list RPAREN
    { UnCall($2, MethId($4), $6) }  

  // skip
  | SKIP
    { Skip }

  // s s
  | statement statement
    { Seq($1, $2) }

// data type

data_type:
  // int
  | INT
    { Int }

  // ClassIDs

  | ID
    { ClassType(ClassId $1) }

data:
  | data_type var COMMA data
    { [($1, $2)] @ $4 } 

  | data_type var data
    { [($1, $2)] @ $3 }

  | data_type var
    { [($1, $2)] }

  | 
    { [] }
// meth

meth:
  // method q(t x, ... , t x) s
  | METHOD ID LPAREN data RPAREN statement
    { Method(MethId($2), $4, $6) }

meth_list:
  | meth meth_list
    { [$1] @ $2 }
  | meth
    { [$1] }

// class definition

cl:
  // class c t x m
  | CLASS ID data meth_list
    { Class(ClassId($2), $3, $4) }

  // class c m
  | CLASS ID meth_list
    { Class(ClassId($2), [], $3) }

  // class c inherits c t x m
  | CLASS ID  INHERITS ID data meth_list
    { ClassInherits(ClassId($2), ClassId($4), $5, $6) }

  // class c inherits c m
  | CLASS ID  INHERITS ID meth_list
    { ClassInherits(ClassId($2), ClassId($4), [], $5) }

class_list:
  | cl class_list
    { [$1] @ $2 }
  | cl
    { [$1] }

// program

prog:
  //cl
  | class_list
    { $1 }
