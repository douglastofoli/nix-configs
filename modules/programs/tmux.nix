{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      programs.tmux = {
        enable = true;

        clock24 = true;
        newSession = true;
        secureSocket = false;

        plugins = with pkgs.tmuxPlugins; [
          sensible
          yank
          resurrect
          continuum
          catppuccin
        ];

        extraConfig = ''
          set -g @resurrect-dir "$HOME/.config/tmux/resurrect"

          set -g @resurrect-save 'S'
          set -g @resurrect-restore 'R'

          set -g @resurrect-strategy-vim 'session'
          set -g @resurrect-strategy-nvim 'session'
          set -g @resurrect-capture-pane-contents 'on'

          set -g @continuum-boot 'off'
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '10'

          set -g @catppuccin_flavor 'macchiato'

          # use 256 colors
          set -g default-terminal "xterm-256color"
          set -ga terminal-overrides ",alacritty:RGB"
          set -ga terminal-overrides '*:Ss=\E[%p1%d q:Se=\E[ q'
          set-environment -g COLORTERM "truecolor"

          # Automatic rename for windows based on the current program
          set-option -g automatic-rename on
          set-option -g automatic-rename-format '#{pane_current_command}'

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
