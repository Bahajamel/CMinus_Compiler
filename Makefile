OCAMLC=ocamlc
MENHIR=menhir
OCAMLLEX=ocamllex

SOURCES=ast.ml parser.ml lexer.ml main.ml
OBJS=$(SOURCES:.ml=.cmo)
EXEC=cminus_parser

all: $(EXEC)

parser.ml parser.mli: parser.mly
	$(MENHIR) parser.mly

lexer.ml: lexer.mll
	$(OCAMLLEX) lexer.mll

%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmo: %.ml
	$(OCAMLC) -c $<

ast.cmo: ast.ml ast.cmi
	$(OCAMLC) -c ast.ml

parser.cmo: parser.ml parser.cmi ast.cmi
	$(OCAMLC) -c parser.ml

lexer.cmo: lexer.ml parser.cmi
	$(OCAMLC) -c lexer.ml

main.cmo: main.ml ast.cmi parser.cmi lexer.cmo
	$(OCAMLC) -c main.ml

$(EXEC): ast.cmi parser.cmi ast.cmo parser.cmo lexer.cmo main.cmo
	$(OCAMLC) -o $(EXEC) ast.cmo parser.cmo lexer.cmo main.cmo

clean:
	rm -f *.cmo *.cmi parser.ml parser.mli lexer.ml $(EXEC)
	rm -f *~

test: $(EXEC)
	./$(EXEC) test.c

.PHONY: all clean test