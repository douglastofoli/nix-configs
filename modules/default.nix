{...}: {
  imports = [
    ./editors/helix/default.nix
    ./programs/alacritty.nix
    ./programs/firefox.nix
    ./programs/tmux.nix
    ./programs/wezterm.nix
    ./shells/git.nix
    ./shells/lf.nix
    ./themes
  ];
}
