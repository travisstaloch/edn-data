let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.aflplusplus
  ];

  shellHook = ''
    export AFL_PATH=$HOME/.afl
    export PATH=$PATH:$HOME/.afl/bin
  '';
}