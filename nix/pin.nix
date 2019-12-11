{ overlays ? [], config ? {} }:
let
    nixPath = builtins.fetchGit {
        url = https://github.com/NixOs/nixpkgs;
        rev = "58fb23f72ad916c8bbfa3c3bc2d0c83c9cfcdd16";
    };
in
import nixPath { overlays = overlays; config = config; }
