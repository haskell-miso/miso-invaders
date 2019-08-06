FROM nixos/nix

RUN nix-env -iA nixpkgs.cabal-install

#RUN nix-env -iA cachix -f https://cachix.org/api/v1/install
#RUN cachix use haskell-miso

COPY . /tmp/miso-invaders
WORKDIR /tmp/miso-invaders
RUN nix-shell --run "echo done"

WORKDIR /root

# docker build -t juliendehos/miso-invaders:latest .
# docker push juliendehos/miso-invaders:latest

