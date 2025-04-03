let
  rust_overlay = import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz");
  pkgs = import (fetchTarball("https://github.com/NixOS/nixpkgs/archive/1750f3c1c89488e2ffdd47cab9d05454dddfb734.tar.gz")) { overlays = [ rust_overlay ]; };
  rust = pkgs.rust-bin.nightly."2025-01-12".default.override {
    extensions = [
      "rust-src" # for rust-analyzer
      "rust-analyzer"
    ];
  };
  buildInputs = [
    rust
  ] ++ (with pkgs; [
    pkg-config

    # perf for cargo-flamegraph
    linuxPackages_latest.perf

    wayland
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXi
    libxkbcommon

    openssl

    vulkan-headers vulkan-loader
  ]);
in
pkgs.mkShell {
  inherit buildInputs;
  RUST_BACKTRACE = 1;
  LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${builtins.toString (pkgs.lib.makeLibraryPath buildInputs)}";
}
