.PHONY: bench
bench:
	opam exec -- dune build @bench --force