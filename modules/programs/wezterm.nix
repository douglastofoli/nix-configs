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

      config.color_scheme = "Dracula (Official)"

      config.tab_bar_at_bottom = true
      config.use_fancy_tab_bar = false
      config.window_decorations = "RESIZE"

      config.font = wezterm.font("JetBrainsMono Nerd Font")

      config.audible_bell = "Disabled"

      return config
    '';

    xdg.configFile."wezterm/dracula.toml".text = ''
      [colors]
      ansi = [
          '#21222c',
          '#ff5555',
          '#50fa7b',
          '#f1fa8c',
          '#bd93f9',
          '#ff79c6',
          '#8be9fd',
          '#f8f8f2',
      ]
      background = '#282a36'
      brights = [
          '#6272a4',
          '#ff6e6e',
          '#69ff94',
          '#ffffa5',
          '#d6acff',
          '#ff92df',
          '#a4ffff',
          '#ffffff',
      ]
      compose_cursor = '#ffb86c'
      cursor_bg = '#f8f8f2'
      cursor_border = '#f8f8f2'
      cursor_fg = '#282a36'
      foreground = '#f8f8f2'
      scrollbar_thumb = '#44475a'
      selection_bg = 'rgba(26.666668% 27.843138% 35.294117% 50%)'
      selection_fg = 'rgba(0% 0% 0% 0%)'
      split = '#6272a4'

      [colors.indexed]

      [colors.tab_bar]
      background = '#282a36'

      [colors.tab_bar.active_tab]
      bg_color = '#bd93f9'
      fg_color = '#282a36'
      intensity = 'Normal'
      italic = false
      strikethrough = false
      underline = 'None'

      [colors.tab_bar.inactive_tab]
      bg_color = '#282a36'
      fg_color = '#f8f8f2'
      intensity = 'Normal'
      italic = false
      strikethrough = false
      underline = 'None'

      [colors.tab_bar.inactive_tab_hover]
      bg_color = '#6272a4'
      fg_color = '#f8f8f2'
      intensity = 'Normal'
      italic = true
      strikethrough = false
      underline = 'None'

      [colors.tab_bar.new_tab]
      bg_color = '#282a36'
      fg_color = '#f8f8f2'
      intensity = 'Normal'
      italic = false
      strikethrough = false
      underline = 'None'

      [colors.tab_bar.new_tab_hover]
      bg_color = '#ff79c6'
      fg_color = '#f8f8f2'
      intensity = 'Normal'
      italic = true
      strikethrough = false
      underline = 'None'

      [metadata]
      aliases = []
      author = 'timescam'
      name = 'Dracula (Official)'
      origin_url = 'https://github.com/dracula/wezterm'
    '';
  };
}
