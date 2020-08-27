all:
	nix-shell --run "cabal build"
	cp `find dist-newstyle -name all.js` public/

clean:
	rm -rf dist-newstyle public/all.js

