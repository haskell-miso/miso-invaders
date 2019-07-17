FROM nixos/nix

RUN nix-channel --add https://nixos.org/channels/nixos-19.03 nixpkgs
RUN nix-channel --update
RUN nix-env -iA nixpkgs.cabal-install

COPY . /tmp/miso-invaders
WORKDIR /tmp/miso-invaders
RUN nix-shell -A env --run "echo done"

WORKDIR /root

# docker build -t juliendehos/miso-invaders:latest .
# docker push juliendehos/miso-invaders:latest

