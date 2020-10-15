{ pkgs ? import <nixpkgs> {} }:

let
  pwd = toString ./.;
in
pkgs.mkShell {
  name = "tabml";
  buildInputs = with pkgs; [
      nodejs-14_x
      jq
      entr
  ];
  shellHook = ''
    export PATH="${pwd}/node_modules/.bin/:$PATH"
  '';
}
