OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=menhir --explain
OCAML=ocaml

all: tiger.cma

parse: all
	${OCAML} tiger.cma parse_me.ml < parse_test.tig

tiger.cma: errorMsg.cmo parser.cmo lexer.cmo
	${OCAMLC} -a -o $@ $+

parser.ml: parser.mly errorMsg.cmi
	${OCAMLYACC} $<

parser.mli: parser.mly
	${OCAMLYACC} $<

lexer.ml: lexer.mll parser.cmi errorMsg.cmi
	${OCAMLLEX} $<

%.cmo: %.ml %.cmi
	${OCAMLC} -c $<

%.cmo: %.ml
	${OCAMLC} -c $<

%.cmi: %.mli
	${OCAMLC} -c $<

%.cmi: %.ml
	${OCAMLC} -c $<

clean:
	-${RM} *.cmo *.cmi parser.ml parser.mli lexer.ml *.cma *.conflicts

.PHONY: all parse clean
