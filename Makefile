build:
	cabal build

run: build
	cabal run

repl: build
	cabal repl
ghci: repl

.PHONY: run build repl ghci