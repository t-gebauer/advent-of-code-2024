{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ocaml
    ocamlPackages.utop
    dune_3
  ];
}
