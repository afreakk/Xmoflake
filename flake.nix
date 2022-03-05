{
  description = "Xmonad & Xmobar built with flake";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [
          (self: super:
            {
              haskellPackages = super.haskellPackages.override (old: {
                overrides = self.lib.composeExtensions (old.overrides or (_: _: { }))
                  (
                    hself: hsuper: {
                      xmonad = hsuper.xmonad_0_17_0;
                      xmonad-contrib = hsuper.xmonad-contrib_0_17_0;
                      xmonad-extras = hsuper.xmonad-extras_0_17_0;
                    }
                  );
              });
            })

          self.overlay
        ];
      });
    in
    {
      overlay = (final: prev: {
        xmoflake = final.haskellPackages.callCabal2nix "xmoflake" (final.nix-gitignore.gitignoreSource [ ".git/" "*.nix" ] ./.) { };
      });
      packages = forAllSystems (system: {
        xmoflake = nixpkgsFor.${system}.xmoflake;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.xmoflake);
      checks = self.packages;
      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in
        haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.xmoflake ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
        });
    };
}
