{
  open Graph_parser
}

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }
| ['0'-'9']+ as lxm { INT (int_of_string lxm) }
| "->"            { ARROW }
| '{'            { LBRACE }
| '}'            { RBRACE }
| ';'            { SEMICOLON }
| ['a'-'z''A'-'Z']+ as lxm { STRING lxm }
| eof            { assert false }
