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
      baseIndex = 1;
      escapeTime = 0;
      newSession = true;

      plugins = with pkgs.tmuxPlugins; [
        fingers
        sensible
        yank
        {
          plugin = resurrect;
          extraConfig = ''
            set -g @ressurect-strategy-vim session
            set -g @ressurect-strategy-nvim session
            set -g @resurrect-capture-pane-contents on
          '';
        }
        {
          plugin = continuum;
          extraConfig = ''
            set -g @continuum-boot on
            set -g @continuum-restore on
            set -g @continuum-save-interval 10
          '';
        }
        {
          plugin = dracula;
          extraConfig = ''
            set -g @dracula-show-battery false
            set -g @dracula-show-powerline true
            set -g @dracula-refresh-rate 10

            set -g @dracula-plugins "git weather"
            set -g @dracula-show-left-icon session
            set -g @dracula-show-empty-plugins false
            set -g @dracula-show-fahrenheit false
          '';
        }
      ];

      extraConfig = ''
        # enable mouse
        set-option -g mouse on

        # set prefix
        unbind C-b
        set -g prefix C-s
        bind C-s send-prefix

        # set vi-mode
        set-window-option -g mode-keys vi

        # select mode and yank commands
        bind-key -T copy-mode-vi v send-keys -X begin-selection
        bind-key -T copy-mode-vi C-v send-keys -X rectangle-toggle
        bind-key -T copy-mode-vi y send-keys -X copy-selection-and-cancel

        # move between pane with vi keybindings
        bind-key h select-pane -L
        bind-key j select-pane -D
        bind-key k select-pane -U
        bind-key l select-pane -R

        # split pane commands
        bind h split-window -h -c "#{pane_current_path}"
        bind v split-window -v -c "#{pane_current_path}"
        bind c new-window -c "#{pane_current_path}"

        # use 256 colors
        set -g default-terminal "xterm-256color"
        set -ga terminal-overrides ",*256col*:Tc"
        set -ga terminal-overrides '*:Ss=\E[%p1%d q:Se=\E[ q'
        set-environment -g COLORTERM "truecolor"
      '';
    };
  };
}
