{ config, inputs, ... }:

let
  host = {
    name = "desktop";
    user.name = "douglas";
    state.version = "25.11";
    system = "x86_64-linux";
    monitors = [
      {
        name = "HDMI-A-2";
        refresh = "74.991";
        x = "0";
        y = "0";
      }
    ];
  };
in
{
  flake.nixosConfigurations.desktop = inputs.nixpkgs.lib.nixosSystem {
    modules = with config.flake.modules.nixos; [
      base
      desktop

      audio
      bluetooth

      sway
      waybar

      neovim

      git
      virtualisation
    ];
  };

  flake.modules.nixos.desktop = {
    inherit host;
    home-manager.users.${host.user.name} = {
      imports = with config.flake.modules.homeManager; [
        mime

        foot

        sway
        waybar
        swaync

        git
      ];
    };
  };
}
