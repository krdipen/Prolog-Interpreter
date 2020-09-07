all:
	@ocamlc -c structure.ml
	@ocamllex lexer.mll
	@ocamlyacc parser.mly
	@ocamlc -c parser.mli
	@ocamlc -c lexer.ml
	@ocamlc -c parser.ml
	@ocamlc -c assignment6.ml
	@ocamlc -o assignment6 structure.cmo lexer.cmo parser.cmo assignment6.cmo
	@./assignment6 input.pl
	@rm lexer.ml
	@rm parser.mli
	@rm parser.ml
	@rm *.cmi
	@rm *.cmo
	@rm assignment6
