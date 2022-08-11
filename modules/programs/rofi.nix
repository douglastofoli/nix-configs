{ config, lib, pkgs, ... }:

let
  inherit (config.lib.formats.rasi) mkLiteral;
  colors = import ../themes/colors.nix;
in {
  programs = {
    rofi = {
      enable = true;
      terminal = "${pkgs.alacritty}/bin/alacritty";
      location = "center";

      theme = with colors.scheme.dracula; {
        "*" = {
          font = "JetBrainsMono Nerd Font 12";
          foreground = mkLiteral "${foreground}";
          background-color = mkLiteral "${background}";
          active-background = mkLiteral "${comment}";
          urgent-background = mkLiteral "${red}";
          selected-background = mkLiteral "@active-background";
          selected-urgent-background = mkLiteral "@urgent-background";
          selected-active-background = mkLiteral "@active-background";
          separatorcolor = mkLiteral "@active-background";
          bordercolor = mkLiteral "@active-background";
        };
        "window" = {
          background-color = mkLiteral "@background-color";
          border = 1;
          border-radius = 6;
          border-color = mkLiteral "@bordercolor";
          padding = 5;
        };
        "mainbox" = {
          border = 0;
          padding = 0;
        };
        "message" = {
          border = mkLiteral "1px dash 0px 0px";
          border-color = mkLiteral "@separatorcolor";
          padding = mkLiteral "1px";
        };
        "textbox" = { text-color = mkLiteral "@foreground"; };
#listview {
    fixed-height: 0;
    border:       2px dash 0px 0px ;
    border-color: @bordercolor;
    spacing:      2px ;
    scrollbar:    false;
    padding:      2px 0px 0px ;
}
#element {
    border:  0;
    padding: 1px ;
}
#element.normal.normal {
    background-color: @background-color;
    text-color:       @foreground;
}
#element.normal.urgent {
    background-color: @urgent-background;
    text-color:       @urgent-foreground;
}
#element.normal.active {
    background-color: @active-background;
    text-color:       @foreground;
}
#element.selected.normal {
    background-color: @selected-background;
    text-color:       @foreground;
}
#element.selected.urgent {
    background-color: @selected-urgent-background;
    text-color:       @foreground;
}
#element.selected.active {
    background-color: @selected-active-background;
    text-color:       @foreground;
}
#element.alternate.normal {
    background-color: @background-color;
    text-color:       @foreground;
}
#element.alternate.urgent {
    background-color: @urgent-background;
    text-color:       @foreground;
}
#element.alternate.active {
    background-color: @active-background;
    text-color:       @foreground;
}
#scrollbar {
    width:        2px ;
    border:       0;
    handle-width: 8px ;
    padding:      0;
}
#sidebar {
    border:       2px dash 0px 0px ;
    border-color: @separatorcolor;
}
#button.selected {
    background-color: @selected-background;
    text-color:       @foreground;
}
#inputbar {
    spacing:    0;
    text-color: @foreground;
    padding:    1px ;
}
#case-indicator {
    spacing:    0;
    text-color: @foreground;
}
#entry {
    spacing:    0;
    text-color: @foreground;
}
#prompt {
    spacing:    0;
    text-color: @foreground;
}
#inputbar {
    children:   [ prompt,textbox-prompt-colon,entry,case-indicator ];
}
#textbox-prompt-colon {
    expand:     false;
    str:        ":";
    margin:     0px 0.3em 0em 0em ;
    text-color: @foreground;
}
element-text, element-icon {
    background-color: inherit;
    text-color: inherit;
}
      };
    };
  };
}
