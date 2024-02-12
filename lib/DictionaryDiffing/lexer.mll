{

open Parser;;
exception Eof;;

}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | ','             { COMMA }
  | ':'             { COLON }
  | '{'             { LBRACKET }
  | '}'             { RBRACKET }
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | ['a'-'z']+ as s { STRING s }
  | eof             { raise Eof }
