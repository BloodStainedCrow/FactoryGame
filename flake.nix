{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=91c9a64ce2a84e648d0cf9671274bb9c2fb9ba60";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, fenix }: let
    pkgs = nixpkgs.legacyPackages."x86_64-linux";
    fenixLib = fenix.packages."x86_64-linux";

    rustToolchain = fenixLib.fromToolchainFile {
      file = ./rust-toolchain.toml;
      sha256 = "sha256-dXoddWaPL6UtPscTpxMUMBDL83jFtqeDtmH/+bXBs3E=";
    };

    neededPackages = with pkgs; [
      wayland
      xorg.libX11
      xorg.libXcursor
      xorg.libXrandr
      xorg.libXi
      libxkbcommon

      openssl

      vulkan-headers vulkan-loader
    ];
  in {
    devShells."x86_64-linux".codium = pkgs.mkShell {
      buildInputs = with pkgs; [
        bashInteractive
        rustToolchain

        perf
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
      ] ++ neededPackages;
      LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${builtins.toString (pkgs.lib.makeLibraryPath neededPackages)}";

      shellHook = ''
        export SHELL="${pkgs.bashInteractive}/bin/bash"
      '';
      

      # env.RUST_SRC_PATH = "${rustToolchain.rust-src}";
    };

    packages."x86_64-linux".default = (pkgs.makeRustPlatform {
      cargo = rustToolchain;
      rustc = rustToolchain;
    }).buildRustPackage {
      name = "factory";
      src = ./.;
      buildInputs = neededPackages;
      nativeBuildInputs = [ pkgs.pkg-config pkgs.makeWrapper ];
      cargoHash = "sha256-Wgk3H7hw9yeMt8juVuLVBX+Z8JnNxD1+e9LY0CTQr0E=";
      # cargoLock.lockFile = ./Cargo.lock;
      doCheck = false;

      postInstall = ''
        wrapProgram "$out/bin/factory" --prefix LD_LIBRARY_PATH : "${builtins.toString (pkgs.lib.makeLibraryPath neededPackages)}"
      '';
    };
  };
}
