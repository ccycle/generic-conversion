{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Specify go-task-3.14.0 for avoiding bugs in the latest version
    nixpkgs-go-task.url = "github:NixOS/nixpkgs/73994921df2b89021c1cbded66e8f057a41568c1";

    flake-utils.url = "github:numtide/flake-utils";

    # TODO: fix stack2cabal in nixpkgs
    stack2cabal.url = "github:hasufell/stack2cabal";
  };

  outputs = { self, nixpkgs, nixpkgs-go-task, flake-utils, stack2cabal }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = { allowUnfree = true; };
        };
        pkgs-go-task = import nixpkgs-go-task {
          inherit system;
          config = { allowUnfree = true; };
        };
        stack2cabalOverrided = removeAttrs stack2cabal [ "nixConfig" ];

        buildInputs = [
          # build tools
          pkgs.stack
          pkgs.cabal-install
          pkgs.haskell.packages.ghc925.haskell-language-server
          pkgs.haskell.packages.ghc8107.haskell-language-server
          # supplementary development tools
          pkgs.nil
          pkgs.nixpkgs-fmt
          stack2cabalOverrided.defaultPackage.${system}
          pkgs.hpack
          pkgs-go-task.go-task
          pkgs.haskellPackages.fourmolu
        ];
      in
      {
        devShell =
          pkgs.mkShell {
            inherit buildInputs;
          };
      }
    );
}
