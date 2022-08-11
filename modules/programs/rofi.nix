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
        "listview" = {
          fixed-height = 0;
          border = mkLiteral "2px dash 0px 0px";
          border-color = mkLiteral "@bordercolor";
          spacing = mkLiteral "2px";
          scrollbar = false;
          padding = mkLiteral "2px 0px 0px";
        };
        "element" = {
          border = 0;
          padding = mkLiteral "1px";
        };
        "element normal normal" = {
          background-color = mkLiteral "@background-color";
          text-color = mkLiteral "@foreground";
        };
        "element normal urgent" = {
          background-color = mkLiteral "@urgent-background";
          text-color = mkLiteral "@urgente-foreground";
        };
        "element normal active" = {
          background-color = mkLiteral "@active-background";
          text-color = mkLiteral "@foreground";
        };
        "element selected normal" = {
          background-color = mkLiteral "@selected-background";
          text-color = mkLiteral "@foreground";
        };
        "element selected urgent" = {
          background-color = mkLiteral "@selected-urgent-background";
          text-color = mkLiteral "@foreground";
        };
        "element selected active" = {
          background-color = mkLiteral "@selected-active-background";
          text-color = mkLiteral "@foreground";
        };
        "element alternate normal" = {
          background-color = mkLiteral "@background-color";
          text-color = mkLiteral "@foreground";
        };
        "element alternate urgent" = {
          background-color = mkLiteral "@urgent-background";
          text-color = mkLiteral "@foreground";
        };
        "element alternate active" = {
          background-color = mkLiteral "@active-background";
          text-color = mkLiteral "@foreground";
        };
        "scrollbar" = {
          width = mkLiteral "2px";
          border = 0;
          handle-width = mkLiteral "8px";
          padding = 0;
        };
        "sidebar" = {
          border = mkLiteral "2px dash 0px 0px";
          border-color = mkLiteral "@separatorcolor";
        };
        "button selected" = {
          background-color = mkLiteral "@selected-background";
          text-color = mkLiteral "@foreground";
        };
        "inputbar" = {
          spacing = 0;
          text-color = mkLiteral "@foreground";
          padding = mkLiteral "1px";
          children =
            mkLiteral "[ prompt, textbox-prompt, entry, case-indicator ]";
        };
        "case-indicator" = {
          spacing = 0;
          text-color = mkLiteral "@foreground";
        };
        "entry" = {
          spacing = 0;
          text-color = mkLiteral "@foreground";
        };
        "prompt" = {
          spacing = 0;
          text-color = mkLiteral "@foreground";
        };
        "textbox-prompt-colon" = {
          expand = false;
          str = mkLiteral ":";
          margin = mkLiteral "0px 0.3em 0em 0em";
          text-color = mkLiteral "@foreground";
        };
        "element-text" = {
          background-color = mkLiteral "inherit";
          text-color = mkLiteral "inherit";
        };
        "element-icon" = {
          background-color = mkLiteral "inherit";
          text-color = mkLiteral "inherit";
        };
      };
    };
  };
}
