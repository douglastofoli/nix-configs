{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      programs.tmux = {
        enable = true;

        clock24 = true;
        newSession = true;
        secureSocket = false;
        terminal = "tmux-256color";

        plugins = with pkgs.tmuxPlugins; [
          sensible
          yank
          resurrect
          continuum
          rose-pine
        ];

        # Opções dos plugins (precisam existir antes dos run-shell)
        extraConfigBeforePlugins = ''
          ##### resurrect
          set -g @resurrect-dir "$HOME/.config/tmux/resurrect"
          set -g @resurrect-save "S"
          set -g @resurrect-restore "R"
          set -g @resurrect-strategy-vim "session"
          set -g @resurrect-strategy-nvim "session"
          set -g @resurrect-capture-pane-contents "on"

          ##### continuum
          set -g @continuum-boot "off"
          set -g @continuum-restore "on"
          set -g @continuum-save-interval "10"

          ##### rose-pine
          set -g @rose_pine_variant "main"

          set -g @rose_pine_host "on" 
          set -g @rose_pine_hostname_short "on"
          set -g @rose_pine_date_time ""
          set -g @rose_pine_user "on"
          set -g @rose_pine_directory "on"
        '';

        extraConfig = ''
          ##### terminal / colors
          set -as terminal-features ",*:RGB"
          set-environment -g COLORTERM "truecolor"

          ##### general
          set -g escape-time 0
          set -g history-limit 50000
          set -g detach-on-destroy off

          ##### mouse
          set -g mouse on

          ##### windows
          set -g base-index 1
          set -g renumber-windows on
          set -g automatic-rename on
          set -g automatic-rename-format "#{pane_current_command}"

          ##### prefix
          unbind C-b
          set -g prefix C-s
          bind C-s send-prefix

          ##### copy mode
          setw -g mode-keys vi
          bind -T copy-mode-vi v send-keys -X begin-selection
          bind -T copy-mode-vi C-v send-keys -X rectangle-toggle
          bind -T copy-mode-vi y send-keys -X copy-selection-and-cancel

          ##### pane navigation
          bind h select-pane -L
          bind j select-pane -D
          bind k select-pane -U
          bind l select-pane -R

          ##### pane resize
          bind -r H resize-pane -L 10
          bind -r J resize-pane -D 10
          bind -r K resize-pane -U 10
          bind -r L resize-pane -R 10

          ##### window navigation
          bind -n S-Left previous-window
          bind -n S-Right next-window
          bind -n M-H previous-window
          bind -n M-L next-window

          ##### split / new window
          bind c new-window -c "#{pane_current_path}"
          bind % split-window -h -c "#{pane_current_path}"
          bind "\"" split-window -v -c "#{pane_current_path}"
        '';
      };
    };
}
