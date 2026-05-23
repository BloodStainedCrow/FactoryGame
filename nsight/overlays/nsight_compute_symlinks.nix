self: super:
{
  cudaPackages = super.cudaPackages // {
    nsight_compute = super.cudaPackages.nsight_compute.overrideAttrs (old: {
      postInstall = old.postInstall + ''
        ln -s $out/bin/target/linux-desktop-glibc_2_11_3-x64 \
          $out/bin/target/linux-desktop-glibc_2_11_3-x86
        ln -s $out/sections $out/bin/sections
      '';

      meta.description = "";
    });
  };
}