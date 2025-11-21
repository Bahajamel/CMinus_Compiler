OCAMLC=ocamlc
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

SOURCES=ast.ml parser.ml lexer.ml scope.ml typing.ml main.ml 
OBJS=$(SOURCES:.ml=.cmo)
EXEC=cminus_parser

all: $(EXEC)

# Génération du parser avec ocamlyacc
parser.ml parser.mli: parser.mly ast.cmi
	$(OCAMLYACC) parser.mly

# Génération du lexer
lexer.ml: lexer.mll
	$(OCAMLLEX) lexer.mll

# Règles génériques
%.cmi: %.mli
	$(OCAMLC) -c $<

%.cmo: %.ml
	$(OCAMLC) -c $<

# Dépendances spécifiques (ordre important)
ast.cmi: ast.mli
	$(OCAMLC) -c ast.mli

ast.cmo: ast.ml ast.cmi
	$(OCAMLC) -c ast.ml

parser.cmi: parser.mli ast.cmi
	$(OCAMLC) -c parser.mli

parser.cmo: parser.ml parser.cmi ast.cmi
	$(OCAMLC) -c parser.ml

lexer.cmo: lexer.ml parser.cmi
	$(OCAMLC) -c lexer.ml

scope.cmo: scope.ml ast.cmi
	$(OCAMLC) -c scope.ml

typing.cmo: typing.ml ast.cmi scope.cmi
	$(OCAMLC) -c typing.ml

main.cmo: main.ml ast.cmi parser.cmi lexer.cmo scope.cmo typing.cmo
	$(OCAMLC) -c main.ml

# Liaison finale
$(EXEC): ast.cmi ast.cmo parser.cmi parser.cmo lexer.cmo scope.cmo typing.cmo main.cmo
	$(OCAMLC) -o $(EXEC) ast.cmo parser.cmo lexer.cmo scope.cmo typing.cmo main.cmo

# Nettoyage
clean:
	rm -f *.cmo *.cmi parser.ml parser.mli lexer.ml $(EXEC)
	rm -f *~

# Test
test: $(EXEC)
	./$(EXEC) test.c

.PHONY: all clean test