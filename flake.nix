{
  description = "A very basic flake";

  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (system:
    let pkgs = nixpkgs.legacyPackages.${system};
        hp = nixpkgs.legacyPackages.${system}.haskellPackages.override (old: {
      overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (f: p: rec {
        reactive-monitor = f.callPackage ./reactive-monitor {};
        reactive-monitor-client = f.callPackage ./reactive-monitor-client {};
        reactive-monitor-common = f.callPackage ./reactive-monitor-common {};
      });
    });
    in {
      packages.reactive-monitor = hp.reactive-monitor;
      devShell = hp.shellFor {
        packages = h: [h.reactive-monitor h.reactive-monitor-client h.reactive-monitor-common];
        withHoogle = false;
        buildInputs = with pkgs; [
          cabal-install
          hp.hlint
          stylish-haskell
          ghcid

          sqlite-interactive

          hp.graphmod

          hp.haskell-language-server

          ffmpeg
        ];
      };
    }
  );
}
