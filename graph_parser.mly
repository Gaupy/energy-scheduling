%{
  open Usr
%}

%token LBRACE RBRACE SEMICOLON ARROW
%token <string> STRING
%token <int> INT

%start parse
%type <Usr.E.t> parse

%%

parse:
| STRING STRING LBRACE nodes edges RBRACE { $5 }
;

nodes:
| { () }
| nodes INT SEMICOLON { $1 }
;

edges:
| { E.empty }
| INT ARROW INT SEMICOLON edges { E.add {id1 = $1; id2 = $3} $5 }
;

%%
