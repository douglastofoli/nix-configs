{ pkgs, ... }:

{
  home.packages = with pkgs; [ polybar ];

  xdg.configFile."polybar".source = ../../dotfiles/polybar;
}
