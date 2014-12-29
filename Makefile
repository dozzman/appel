OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

all: tiger.cma

tiger.cma: parser.cmo errorMsg.cmo lexer.cmo
	${OCAMLC} -a -o $@ $+
parser.ml: parser.mly
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
	-${RM} *.cmo *.cmi parser.ml parser.mli lexer.ml
