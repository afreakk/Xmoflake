{
  inputs = {
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, unstable, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = unstable.legacyPackages.${system};
          hl = pkgs.haskell.lib.compose;
          haskellPackages = pkgs.haskellPackages;
          trv = unstable.lib.trivial;
          project = extraModifiers: returnShellEnv:
            haskellPackages.developPackage {
              root = unstable.lib.sourceFilesBySuffices ./. [ ".yaml" ".hs" ];
              name = "XmoFlake";
              returnShellEnv = returnShellEnv;
              modifier =
                (trv.flip trv.pipe) ([
                  hl.enableStaticLibraries
                  hl.justStaticExecutables
                  hl.disableExecutableProfiling
                ] ++ extraModifiers);
            };
        in
        {
          packages.default = project [ ] false;
          devShell = project
            [
              (hl.addBuildTools (with haskellPackages; [
                haskell-language-server
                hlint
              ]))
              (hl.overrideCabal (old: {
                shellHook = (old.shellHook or "") + ''
                  echo "Generating .cabal file from package.yaml using hpack, remember to regenerate if you change package.yaml! Although nix build dont care about it, its mostly for helping haskell-language-server." | ${pkgs.cowsay}/bin/cowsay
                  hpack
                '';
              }))
            ]
            true;
        }) // {
      overlay = final: prev: {
        xmoflake = self.packages.${final.system}.default;
      };
    };
}
