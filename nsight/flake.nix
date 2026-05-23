{
  inputs = {
    nixpkgs.url  = "github:NixOS/nixpkgs/nixos-25.05";
  };

  outputs = {self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      myOverlays = [
        (import ./overlays/nsight_compute_symlinks.nix)
      ];
      pkgs = import nixpkgs {
        inherit system;
        overlays = myOverlays;
      };
  in
    {
      devShell.x86_64-linux = pkgs.mkShell {
        nativeBuildInputs = [
          pkgs.cudaPackages.nsight_compute
        ];
      };
    };
}