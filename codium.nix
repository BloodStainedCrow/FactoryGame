let
  nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
  pkgs = import (fetchTarball("https://github.com/NixOS/nixpkgs/archive/929116e316068c7318c54eb4d827f7d9756d5e9c.tar.gz")) { };
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