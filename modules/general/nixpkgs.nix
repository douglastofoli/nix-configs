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
              url = "https://downloads.cursor.com/production/60faf7b51077ed1df1db718157bbfed740d2e168/linux/x64/Cursor-2.6.13-x86_64.AppImage";
              sha256 = "sha256-eqOZQhIWRhfBOB4owcLDt1fI9YAu4uRYlw6HEHutTMk=";
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
