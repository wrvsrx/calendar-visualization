{
  description = "calendar visualization";

  inputs = {
    flake-lock.url = "github:wrvsrx/flake-lock";
    nixpkgs.follows = "flake-lock/nixpkgs";
    flake-parts.follows = "flake-lock/flake-parts";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } ({ inputs, ... }: {
    systems = [ "x86_64-linux" ];
    perSystem = { system, pkgs, ... }: rec {
      packages.default = pkgs.haskellPackages.callPackage ./default.nix { };
      devShells.default = pkgs.mkShell {
        inputsFrom = [ packages.default.env ];
        nativeBuildInputs = [
          pkgs.haskellPackages.cabal-fmt
          pkgs.haskellPackages.cabal2nix
          pkgs.haskell-language-server
        ];
      };
      formatter = pkgs.nixpkgs-fmt;
    };
  });
}
