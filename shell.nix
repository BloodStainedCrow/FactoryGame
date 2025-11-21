let
  pkgs = import (fetchTarball("https://github.com/NixOS/nixpkgs/archive/91c9a64ce2a84e648d0cf9671274bb9c2fb9ba60.tar.gz")) { overlays = [  ]; };
  buildInputs = [
  ] ++ (with pkgs; [
    rustup

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
