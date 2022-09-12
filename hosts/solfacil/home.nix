{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      asdf-vm
      bat
      bpytop
      exa
      feh
      gnome.file-roller
      gnome.seahorse
      insomnia
      jetbrains.datagrip
      obsidian
      pcmanfm
      slack
    ];
  };

  programs.alacritty.settings.font.size = 12;
  programs.git.signing.key = "A78F1B85248F4095";

  services.blueman-applet.enable = true;
}
