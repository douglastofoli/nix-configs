{ withSystem, ... }:

{
  flake.modules.nixos.base =
    { config, ... }:
    {
      nixpkgs.pkgs = withSystem config.nixpkgs.hostPlatform.system ({ pkgs, ... }: pkgs);
      nixpkgs.hostPlatform = config.host.system;

      nixpkgs.overlays = [
        (self: super: {
          code-cursor = super.appimageTools.wrapType2 {
            pname = "cursor";
            version = "2.5.20";

            src = super.fetchurl {
              url = "https://downloads.cursor.com/production/b29eb4ee5f9f6d1cb2afbc09070198d3ea6ad76f/linux/x64/Cursor-2.6.20-x86_64.AppImage";
              sha256 = "sha256-fEvDNnFdJ2WhFam6tw1rnDbNQEZmxsoraIuvrHuKy+w=";
            };
          };
        })
      ];
    };

  flake.modules.darwin.base =
    { config, ... }:
    {
      nixpkgs.pkgs = withSystem config.nixpkgs.hostPlatform.system ({ pkgs, ... }: pkgs);
      nixpkgs.hostPlatform = config.host.system;
    };
}
