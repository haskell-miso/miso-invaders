FROM nixos/nix

RUN nix-env -iA nixpkgs.cabal-install

#RUN nix-env -iA cachix -f https://cachix.org/api/v1/install
#RUN cachix use haskell-miso

COPY . /tmp/miso-invaders
WORKDIR /tmp/miso-invaders
RUN nix-shell --run "echo done"

WORKDIR /root

# docker build -t juliendehos/cicd:miso-invaders -f Dockerfile ..
# docker push juliendehos/cicd:miso-invaders

