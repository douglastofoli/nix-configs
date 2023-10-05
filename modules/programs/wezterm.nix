{ pkgs, vars, ... }:

{
  home-manager.users.${vars.user} = {
    home.packages = [ pkgs.wezterm ];

    xdg.configFile."wezterm/wezterm.lua".text = ''
      local wezterm = require("wezterm")  

      local config = {}

      -- In newer versions of wezterm, use the config_builder which will
      -- help provide clearer error messages
      if wezterm.config_builder then
        config = wezterm.config_builder()
      end

      config.color_scheme = "Catppuccin Mocha"

      config.tab_bar_at_bottom = true
      config.use_fancy_tab_bar = false
      config.window_decorations = "RESIZE"

      config.font = wezterm.font("JetBrainsMono Nerd Font")

      config.audible_bell = "Disabled"

      return config
    '';
  };
}
