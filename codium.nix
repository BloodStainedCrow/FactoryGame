let
  nix-vscode-extensions.url = "github:nix-community/nix-vscode-extensions";
  pkgs = import (fetchTarball("https://github.com/NixOS/nixpkgs/archive/91c9a64ce2a84e648d0cf9671274bb9c2fb9ba60.tar.gz")) { };
  addr2linePkg = pkgs.callPackage ./addr2line-rs/default.nix {};
in
pkgs.mkShell {
  buildInputs = [
  ] ++ (with pkgs; [
    bacon
    openssl

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