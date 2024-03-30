{
  custom-config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf types;
  cfg = custom-config.tmux;
in {
  options.tmux = {
    enable = mkEnableOption {
      description = "Enables Tmux terminal multiplexer.";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    programs.tmux = {
      inherit (cfg) enable;
      clock24 = true;

      plugins = with pkgs.tmuxPlugins; [
        sensible
        yank
        {
          plugin = dracula;
          extraConfig = ''
            set -g @dracula-show-battery false
            set -g @dracula-show-powerline true
            set -g @dracula-refresh-rate 10

            set -g @dracula-plugins "git time weather"
            set -g @dracula-show-empty-plugins false
            set -g @dracula-show-timezone false
            set -g @dracula-time-format "%a %m/%d/%y"
          '';
        }
      ];

      extraConfig = ''
        set -g mouse on
        set -g prefix C-s
        set -g default-terminal "tmux-256color"

        set-option -a terminal-overrides ",*256col*:RGB"

        bind-key h select-pane -L
        bind-key j select-pane -D
        bind-key k select-pane -U
        bind-key l select-pane -R
      '';
    };
  };
}
