{
  inputs,
  ...
}:

{
  flake.modules.nixos.base =
    { config, pkgs, ... }:
    {
      imports = [
        inputs.stylix.nixosModules.stylix
      ];

      stylix = {
        enable = true;
        base16Scheme = "${pkgs.base16-schemes}/share/themes/seti.yaml";
        polarity = "dark";
        cursor = {
          package = pkgs.dracula-theme;
          name = "Dracula-cursors";
          size = 16;
        };
        fonts = {
          serif = {
            package = pkgs.dejavu_fonts;
            name = "DejaVu Serif";
          };

          sansSerif = {
            package = pkgs.dejavu_fonts;
            name = "DejaVu Sans";
          };

          monospace = {
            package = pkgs.nerd-fonts.fira-code;
            name = "FiraCode Nerd Font Mono";
          };

          emoji = {
            package = pkgs.noto-fonts-color-emoji;
            name = "Noto Color Emoji";
          };
        };
        icons = {
          enable = true;
          package = pkgs.dracula-icon-theme;
          dark = "Dracula";
          light = "Dracula";
        };
        targets = {
          gnome.enable = false;
          nixvim.enable = false;
        };
      };
    };
}
