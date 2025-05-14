let
  rust_overlay = import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz");
  pkgs = import (fetchTarball("https://github.com/NixOS/nixpkgs/archive/929116e316068c7318c54eb4d827f7d9756d5e9c.tar.gz")) { overlays = [ rust_overlay ]; };
  rust = pkgs.rust-bin.nightly."2025-04-10".default.override {
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
