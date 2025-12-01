{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = (with pkgs; [
      nixd
      ocaml
      dune_3
      ocamlformat
      ocamlPackages.ocaml-lsp
      ocamlPackages.utop
  ]);
}
