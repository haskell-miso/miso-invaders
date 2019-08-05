
all:
	cabal build
	ln -sf ../dist/build/miso-invaders/miso-invaders.jsexe/all.js public/
	@echo "Done. You can open public/index.html in a browser."

clean:
	rm -rf dist public/all.js

