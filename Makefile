OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
OCAMLDEP=ocamldep

OBJS=const.cmi const.cmo usr.cmi usr.cmo graph_parser.mli graph_parser.ml graph_parser.cmi graph_parser.cmo graph_lexer.ml graph_lexer.cmo doc.cmo schedule.cmo heur.cmo script.cmo
OPTOBJS=const.cmi const.cmx usr.cmi usr.cmx graph_parser.mli graph_parser.ml graph_parser.cmi graph_parser.cmx graph_lexer.ml graph_lexer.cmx doc.cmx schedule.cmx heur.cmx script.cmx

CMOBJS=const.cmo usr.cmo graph_parser.cmo graph_lexer.cmo doc.cmo schedule.cmo heur.cmo script.cmo
CMOPTOBJS=const.cmx usr.cmx graph_parser.cmx graph_lexer.cmx doc.cmx schedule.cmx heur.cmx script.cmx

all : dep comp

opt: dep optcomb

comp : $(OBJS)
	$(OCAMLC) -o comp unix.cma $(CMOBJS)

optcomb: $(OPTOBJS)
	$(OCAMLOPT) -o comp unix.cmxa $(CMOPTOBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mly .mll

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

.mly.ml:
	$(OCAMLYACC) $<

.mly.mli:
	$(OCAMLYACC) $<
.mll.ml:
	$(OCAMLLEX) $<

dep:
	$(OCAMLDEP) *.ml *.mli > .dep

clean:
	rm -f *.o *.cmx *.cmo *.cmi *.out *.pdf *.toc *.blg *.bbl *.lot graph_parser.ml graph_parser.mli graph_lexer.ml

include .dep
