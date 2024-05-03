{
  custom-config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption mkIf types;
  cfg = custom-config.tmux;
in {
  options.tmux = {
    enable = mkEnableOption {
      description = "Enables Tmux terminal multiplexer.";
      type = types.bool;
      default = false;
    };

    tmuxinator = mkOption {
      description = "Manage complex tmux sessions.";
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    programs.tmux = {
      inherit (cfg) enable;
      tmuxinator.enable = cfg.tmuxinator;

      clock24 = true;
      escapeTime = 0;
      newSession = true;
      secureSocket = false;

      plugins = with pkgs.tmuxPlugins; [
        sensible
        yank
        {
          plugin = resurrect;
          extraConfig = ''
            set -g @resurrect-dir $HOME/.config/tmux/resurrect

            set -g @resurrect-save S
            set -g @resurrect-restore R

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

            set -g @continuum-boot-options alacritty
          '';
        }
        {
          plugin = dracula;
          extraConfig = ''
            set -g @dracula-show-battery false
            set -g @dracula-show-powerline true
            set -g @dracula-refresh-rate 10

            set -g @dracula-plugins "git"
            set -g @dracula-git-colors "pink dark_gray"
            set -g @dracula-show-left-icon session
            set -g @dracula-show-empty-plugins false
          '';
        }
      ];

      extraConfig = ''
        # use 256 colors
        set -g default-terminal "xterm-256color"
        set -ga terminal-overrides ",*256col*:Tc"

        # configs
        set -g escape-time 0
        set -g history-limit 50000
        set -g detach-on-destroy off

        # enable mouse
        set-option -g mouse on

        # change tab index
        set -g base-index 1
        set -g renumber-windows on

        # set prefix
        unbind C-b
        set -g prefix C-s
        bind C-s send-prefix

        # refresh config
        unbind r
        bind r source-file ~/.config/tmux/tmux.conf

        # vi mode
        setw -g mode-keys vi

        # select mode and yank commands
        bind -T copy-mode-vi v send-keys -X begin-selection
        bind -T copy-mode-vi C-v send-keys -X rectangle-toggle
        bind -T copy-mode-vi y send-keys -X copy-selection-and-cancel

        # select window with hjkl
        bind h select-pane -L
        bind j select-pane -D
        bind k select-pane -U
        bind l select-pane -R

        # resize window with HJKL
        bind -r H resize-pane -L 10
        bind -r J resize-pane -D 10
        bind -r K resize-pane -U 10
        bind -r L resize-pane -R 10

        # shift arrow to switch windows
        bind -n S-Left previous-window
        bind -n S-Right next-window

        # shift alt vim keys to switch windows
        bind -n M-H previous-window
        bind -n M-L next-window

        # split pane commands
        bind c new-window -c "#{pane_current_path}"
        bind % split-window -h -c "#{pane_current_path}"
        bind '"' split-window -v -c "#{pane_current_path}"
      '';
    };
  };
}
