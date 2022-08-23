{
  programs.i3status-rust = {
    enable = true;

    bars = {
      main = {
        blocks = [
          {
            block = "focused_window";
            show_marks = "visible";
            max_width = 26;
          }
          {
            block = "docker";
            interval = 5;
            format = "{running}/{total}";
            icons_overrides = {
              docker = "";
            };
          }
          {
            block = "music";
            player = "spotify";
            buttons = ["play"] ++ ["next"];
            dynamic_width = true;
            hide_when_empty = true;
            max_width = 26;
            icons_overrides = {
              spotify = "";
            };
          }
          {
            block = "sound";
          }
          {
            block = "time";
            interval = 5;
            format = "%a %d/%m %R";
            theme_overrides = {
              idle_bg = "#7aa2f7";
              idle_fg = "#f8f8f2";
            };
          }
        ];
        icons = "awesome5";
        theme = "dracula";
      };
    };
  };
}