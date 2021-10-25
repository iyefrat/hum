{
  description = "A very minimal flake for building and running hum";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.hum = pkgs.haskellPackages.callPackage ./default.nix { };
        defaultPackage = self.packages.${system}.hum;

        apps.hum = flake-utils.lib.mkApp { drv = self.defaultPackage.${system}; name = "hum"; };
        defaultApp = self.apps.${system}.hum;
      }
    );
}
