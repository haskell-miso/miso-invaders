with (import (builtins.fetchTarball {
  # miso 1.7.1
  url = "https://github.com/dmjio/miso/archive/bb23019.tar.gz";
  sha256 = "0q44lxzz8pp89ccaiw3iwczha8x2rxjwmgzkxj8cxm97ymsm0diy";
}) {});
(pkgs.haskell.packages.ghcjs.callCabal2nix "miso-invaders" ./. {}).env

