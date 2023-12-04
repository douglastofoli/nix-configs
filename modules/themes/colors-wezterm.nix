{colors, ...}:
with colors.scheme.dracula; {
  dracula = {
    ansi = [
      "${black}"
      "${red}"
      "${green}"
      "${yellow}"
      "${purple}"
      "${pink}"
      "${cyan}"
      "${foreground}"
    ];
    background = "${background}";
    brights = [
      "${comment}"
      "#ff6e6e"
      "#69ff94"
      "#ffffa5"
      "#d6acff"
      "#ff92df"
      "#a4ffff"
      "#ffffff"
    ];
    compose_cursor = "${orange}";
    cursor_bg = "${white}";
    cursor_border = "${white}";
    cursor_fg = "${background}";
    foreground = "${white}";
    scrollbar_thumb = "${current_line}";
    selection_bg = "rgba(26.666668% 27.843138% 35.294117% 50%)";
    selection_fg = "rgba(0% 0% 0% 0%)";
    split = "${comment}";
    tab_bar = {
      background = "${background}";
      active_tab = {
        bg_color = "${purple}";
        fg_color = "${background}";
        intensity = "Normal";
        italic = false;
        strikethrough = false;
        underline = "None";
      };
      inactive_tab = {
        bg_color = "${background}";
        fg_color = "${white}";
        intensity = "Normal";
        italic = false;
        strikethrough = false;
        underline = "None";
      };
      inactive_tab_hover = {
        bg_color = "${comment}";
        fg_color = "${white}";
        intensity = "Normal";
        italic = true;
        strikethrough = false;
        underline = "None";
      };
      new_tab = {
        bg_color = "${background}";
        fg_color = "${white}";
        intensity = "Normal";
        italic = false;
        strikethrough = false;
        underline = "None";
      };
      new_tab_hover = {
        bg_color = "${pink}";
        fg_color = "${white}";
        intensity = "Normal";
        italic = true;
        strikethrough = false;
        underline = "None";
      };
    };
  };
}
