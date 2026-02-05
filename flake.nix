{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=91c9a64ce2a84e648d0cf9671274bb9c2fb9ba60";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
  };

  inputs.self.lfs = true;
  outputs = { self, nixpkgs, fenix, crane }: let
    inherit (nixpkgs) lib;
    pkgs = nixpkgs.legacyPackages."x86_64-linux";
    fenixLib = fenix.packages."x86_64-linux";

    toolchain_sha = "sha256-dXoddWaPL6UtPscTpxMUMBDL83jFtqeDtmH/+bXBs3E=";

    rustToolchain = fenixLib.fromToolchainFile {
      file = ./rust-toolchain.toml;
      sha256 = toolchain_sha;
    };

    wasmToolchain = fenixLib.combine [
      (fenixLib.targets.wasm32-unknown-unknown.fromToolchainFile {
        file = ./rust-toolchain.toml;
        sha256 = toolchain_sha;
      })
      rustToolchain
    ];

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

    client_package_for_target = {
      target, toolchain
    }: ((crane.mkLib nixpkgs.legacyPackages.${pkgs.system}).overrideToolchain toolchain).buildPackage {
      name = "factory";
      CARGO_BUILD_TARGET = target;
      meta = {
        homepage = "https://www.github.com/BloodStainedCrow/FactoryGame/";
        maintainers = with lib.maintainers; [ BloodStainedCrow ];
        mainProgram = "factory";
      };
      src = ./.;

      buildInputs = neededPackages;
      nativeBuildInputs = [ pkgs.pkg-config pkgs.makeWrapper ];
      cargoHash = "sha256-83+1Y486PUHM9+uyFw+yJ9bNMlMbN/fc8cYRzKmDdb8=";
      # cargoLock.lockFile = ./Cargo.lock;
      doCheck = false;

      postInstall = ''
        wrapProgram "$out/bin/factory" --prefix LD_LIBRARY_PATH : "${builtins.toString (pkgs.lib.makeLibraryPath neededPackages)}"
      '';
    };

    client_package = client_package_for_target { target = "x86_64-unknown-linux-gnu"; toolchain = rustToolchain; };
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

    packages."x86_64-linux".default = client_package;
    packages."x86_64-linux".dedicated_server = client_package.overrideAttrs ( oldAttrs: { cargoBuildFlags = [ "--no-default-features" "-F logging" ]; });

    "wasm" = (client_package_for_target { target = "wasm32-unknown-unknown"; toolchain = wasmToolchain; }).overrideAttrs ( oldAttrs: { 
      nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.wabt pkgs.binaryen ];
      postInstall = ''
        mkdir -p $out/lib
        ls -lR $out
        wasm-strip $out/bin/factory.wasm -o $out/lib/factory.wasm
        # TODO: Use wasm-opt
        # wasm-opt $out/lib/factory.wasm -o $out/lib/factory.wasm -O4
        rm -r $out/bin
        wasm-validate $out/lib/factory.wasm
      '';

      # We cannot run tests on a wasm binary
      doCheck = false;
    });

    "trunk" = ((crane.mkLib nixpkgs.legacyPackages.${pkgs.system}).overrideToolchain wasmToolchain).buildTrunkPackage {
      nativeBuildInputs = [ pkgs.lld ];
      CARGO_BUILD_TARGET = "wasm32-unknown-unknown";
      src = ./.;

      wasm-bindgen-cli = pkgs.buildWasmBindgenCli rec {
        src = pkgs.fetchCrate {
          pname = "wasm-bindgen-cli";
          version = "0.2.106";
          hash = "sha256-M6WuGl7EruNopHZbqBpucu4RWz44/MSdv6f0zkYw+44=";
          # hash = lib.fakeHash;
        };

        cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
          inherit src;
          inherit (src) pname version;
          hash = "sha256-ElDatyOwdKwHg3bNH/1pcxKI7LXkhsotlDPQjiLHBwA=";
          # hash = lib.fakeHash;
        };
      };
    };

  };
}
