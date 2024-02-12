%{

open Dict

%}

%token <int> INT
%token <string> STRING
%token LBRACKET RBRACKET COMMA COLON
		    
%start main

%type <Dict.dict> main

%%

main:
  | LBRACKET value RBRACKET { $2 }
;

value:
  | pair COMMA value  { Hashtbl.add $3 (fst $1) (snd $1); $3 }
  | pair              {
                        let d = (Hashtbl.create 1) in
                        Hashtbl.add d (fst $1) (snd $1); d
                      }
;  

pair:
  | STRING COLON val_type   { ($1, $3) }
;

val_type:
  | STRING                  { Dict.String $1 }
  | INT                     { Dict.Int $1 }
  | LBRACKET value RBRACKET { Dict.Dict $2 }
;

