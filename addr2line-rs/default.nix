{ lib, rustPlatform, fetchCrate }:

rustPlatform.buildRustPackage rec {
  pname = "addr2line-rs";
  version = "0.25.0";

  src = fetchCrate {
    pname = "addr2line";
    version = "0.25.0";
    hash = "sha256-ZDgASG5pLbavXJFoJnuSq+Y4dX9vRo/IOhbTij5kOuI=";
  };

  cargoHash = "sha256-cdWOqGL5f1v4j0OVd/wAF9UwR7ELF8ysAvgPROoM5qc=";

  useFetchCargoVendor = true;

  cargoFeatures = [ "bin" ];

dontBuild = true;

  installPhase = ''
    mkdir -p $out/bin
    cargo install --path . --root $out --features bin --locked
'';


}