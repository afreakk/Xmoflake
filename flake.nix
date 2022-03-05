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
          self.overlay
        ];
      });
    in
    {
      overlay = (final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
            (
              hfinal: hprev: {
                xmonad = hprev.xmonad_0_17_0;
                xmonad-contrib = hprev.xmonad-contrib_0_17_0;
                xmonad-extras = hprev.xmonad-extras_0_17_0;
              }
            );
        });
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
