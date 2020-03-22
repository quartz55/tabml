{ pkgs ? import <nixpkgs> {} }:

let
  PROJECT_ROOT = toString ./.;
in
pkgs.mkShell {
  name = "tabml";
  buildInputs = with pkgs; [
      nodejs-13_x
  ];
  shellHook = ''
    export PATH="${PROJECT_ROOT}/node_modules/.bin/:$PATH"
  '';
}
