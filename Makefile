
.PHONY= update build optim

all: update build optim

update:
	wasm32-wasi-cabal update

build:
	wasm32-wasi-cabal build 
	rm -rf public
	cp -r static public
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(shell wasm32-wasi-cabal list-bin miso-invaders) --output public/ghc_wasm_jsffi.js
	cp -v $(shell wasm32-wasi-cabal list-bin miso-invaders) public/

optim:
	wasm-opt -all -O2 public/miso-invaders.wasm -o public/miso-invaders.wasm
	wasm-tools strip -o public/miso-invaders.wasm public/miso-invaders.wasm

serve:
	http-server public

