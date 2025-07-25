let
  nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
  pkgs = import (fetchTarball("https://github.com/NixOS/nixpkgs/archive/1750f3c1c89488e2ffdd47cab9d05454dddfb734.tar.gz")) { };
  addr2linePkg = pkgs.callPackage ./addr2line-rs/default.nix {};
in
pkgs.mkShell {
  buildInputs = [
  ] ++ (with pkgs; [
    bacon

    (vscode-with-extensions.override {
    vscode = vscodium;
    vscodeExtensions = with vscode-extensions; [
      rust-lang.rust-analyzer
      vadimcn.vscode-lldb
      gruntfuggly.todo-tree
      a5huynh.vscode-ron
    ];
  })

  addr2linePkg
  ]);
  RUST_BACKTRACE = 1;
  
  shellHook = ''
    export PATH=${addr2linePkg}/bin:$PATH
  '';
}