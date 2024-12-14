{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ocaml
    ocamlPackages.utop
    ocamlPackages.ppx_expect
    ocamlPackages.ppx_deriving
    dune_3
  ];
}
