{ pkgs ? import ./nixpkgs.pinned.nix
}: drv: with drv.hPkgs; let
  name = drv.name;
  ghc = ghcWithPackages (pkgs: (drv.propagatedBuildInputs ++ drv.buildInputs));
  cabalShim = pkgs.writeScriptBin "cabal" ''
    trap cleanup SIGINT
    cleanup() {
      rm -f ${name}.cabal
    }
    ${hpack}/bin/hpack && PATH=${ghc}/bin:$PATH ${cabal-install}/bin/cabal $@
    cleanup
  '';
  repl = pkgs.writeScriptBin "repl" ''
    ${cabalShim}/bin/cabal repl $@
  '';
  watch = pkgs.writeScriptBin "watch" ''
    ${pkgs.ghcid}/bin/ghcid -c "${repl}/bin/repl $@"
  '';
in pkgs.mkShell {
  inherit name;
  buildInputs = [
    cabalShim
    repl
    watch
  ];
  shellHook = ''
    HISTFILE=${toString ../.history}
    export LOCAL_HISTFILE=${toString ../.history}
  '';
}

