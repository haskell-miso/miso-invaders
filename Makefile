
all:
	cabal build miso-invaders
	cp dist/build/miso-invaders/miso-invaders.jsexe/all.js public/

clean:
	rm -rf dist public/all.js

