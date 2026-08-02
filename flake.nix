{
  description = "Emacs major mode for TypeSpec (using tree-sitter)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    tree-sitter-typespec = {
      type = "github";
      owner = "happenslol";
      repo = "tree-sitter-typespec";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      tree-sitter-typespec,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let pkgs = import nixpkgs { inherit system; };
      in
      {
        packages = rec {
          default = typespec-ts-mode;

          typespec-ts-grammar = pkgs.tree-sitter.buildGrammar {
            language = "typespec";
            version = tree-sitter-typespec.rev;
            src = tree-sitter-typespec;
          };

          typespec-ts-mode = (pkgs.emacsPackagesFor pkgs.emacs30).trivialBuild {
            pname = "typespec-ts-mode";
            version = "0.2";
            src = ./.;
          };
        };
      }
    );
}
