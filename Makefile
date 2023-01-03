
.POSIX:
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .mll .mly
.PHONY: all doc clean distclean


OCAMLC=ocamlc
OCAMLFLAGS=-w A
LIBS=
OBJS=ast.cmo parser.cmo lexer.cmo comp.cmo
BIN=comp



all: $(BIN)


$(BIN): $(OBJS)
	$(OCAMLC) $(OCAMLFLAGS) $(LIBS) $(OBJS) -o $@


.mll.ml:
	ocamllex $< -q -o $@

.mly.ml:
	ocamlyacc -v $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@


clean:
	@rm -vf *.cm[io] *~ .*~ parser.ml parser.mli lexer.ml lexer.mli

distclean: clean
	@rm -fr doc $(BIN)

parser.mli: parser.ml
ast.cmo: ast.cmi
parser.cmo: ast.cmi parser.cmi
lexer.cmi: lexer.ml
lexer.cmo: parser.cmi lexer.cmi
comp.cmo: ast.cmi parser.cmi lexer.cmo
