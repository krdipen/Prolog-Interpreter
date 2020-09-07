%{
open Structure
%}
%token LPAREN RPAREN COMMA COLON DOT INVALID EOF
%token <string> VARIABLE
%token <string> CONSTANT
%start main
%type <Structure.clause> main
%%
main : CLAUSE DOT {$1}
     | EOF {(C("EOF"),[])} ;
CLAUSE : HEAD {($1,[])}
       | HEAD COLON BODY {($1,$3)} ;
HEAD : LPAREN HEAD RPAREN {$2}
     | CONSTANT {C($1)}
     | CONSTANT LPAREN TS RPAREN {Node($1,$3)} ;
TS : T {[$1]}
   | T COMMA TS {$1::$3} ;
T : LPAREN T RPAREN {$2}
  | VARIABLE {V($1)}
  | CONSTANT {C($1)}
  | CONSTANT LPAREN TS RPAREN {Node($1,$3)} ;
BODY : HEAD {[$1]}
     | HEAD COMMA BODY {$1::$3} ;
