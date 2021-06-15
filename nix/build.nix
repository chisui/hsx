{ pkgs ? import ./nixpkgs.pinned.nix 
}: let
  hPkgs = pkgs.haskell.packages.ghc901;
in { inherit hPkgs; } // hPkgs.callCabal2nix "hsx" ../. {}

