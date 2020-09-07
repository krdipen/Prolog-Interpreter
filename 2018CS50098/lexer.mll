{
open Parser
}
rule token = parse
    |   '('                                                 { LPAREN }
    |   ')'                                                 { RPAREN }
    |   ','                                                 { COMMA }
    |   ':'                                                 { COLON }
    |   '.'                                                 { DOT }
    |   ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*    as text     { VARIABLE (text) }
    |   ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']*    as text     { CONSTANT (text) }
    |   [' ''\t''\n']+                                      { token lexbuf }
    |   [^' ''\t''\n''('')'','':''.']+                      { INVALID }
    |   eof                                                 { EOF }
