{
  description = "sutra of microlens";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig.bash-prompt-prefix = "\\e[33m[dev]\\e[0m ";
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = import ./nix/shell.nix { inherit pkgs; };
      }
    );

}
