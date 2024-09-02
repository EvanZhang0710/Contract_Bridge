build:
	dune build
	
code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

game:
	OCAMLRUNPARAM=b dune exec bin/main.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

.PHONY: test

zip:
	rm -f contract_bridge.zip
	zip -r contract_bridge.zip . 

clean:
	dune clean
	rm -f contract_bridge.zip

doc:
	dune build @doc
