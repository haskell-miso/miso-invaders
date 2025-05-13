all:
	nix develop --command bash -c "./build.sh"
	http-server public

clean:
	rm -rf dist-newstyle public

