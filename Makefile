.PHONY: bench
bench:
	dune clean
	EIO_BACKEND=luv opam exec -- dune build --profile release @benche --force --no-buffer

.PHONY: docs
docs:
	dune build @doc
	open _build/default/_doc/_html/index.html