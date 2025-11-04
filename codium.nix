let
  nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
  pkgs = import (fetchTarball("https://github.com/NixOS/nixpkgs/archive/1750f3c1c89488e2ffdd47cab9d05454dddfb734.tar.gz")) { };
in
pkgs.mkShell {
  buildInputs = [
  ] ++ (with pkgs; [

    (vscode-with-extensions.override {
    vscode = vscodium;
    vscodeExtensions = with vscode-extensions; [
      rust-lang.rust-analyzer
      vadimcn.vscode-lldb
      gruntfuggly.todo-tree
    ];
  })
  ]);
  RUST_BACKTRACE = 1;
}