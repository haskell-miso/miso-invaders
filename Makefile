
.PHONY= update build optim

all: update build optim

update:
	wasm32-wasi-cabal update

build:
	wasm32-wasi-cabal build 
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin miso-invaders | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/miso-invaders.wasm -o public/miso-invaders.wasm
	wasm-tools strip -o public/miso-invaders.wasm public/miso-invaders.wasm

serve:
	http-server public

clean:
	rm -rf dist-newstyle public

