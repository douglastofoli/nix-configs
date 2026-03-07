{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      fonts = {
        packages = with pkgs; [
          cantarell-fonts
          corefonts # Microsoft fonts
          font-awesome
          liberation_ttf
          mononoki
          noto-fonts
          noto-fonts-cjk-sans
          noto-fonts-color-emoji
          roboto
          ubuntu-classic
          nerd-fonts.jetbrains-mono
          nerd-fonts.ubuntu-mono
        ];
      };
    };
}
