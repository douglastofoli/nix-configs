(setq user-full-name "douglastofoli"
      user-mail-address "tofoli.douglas@hotmail.com")

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 24))

(setq doom-theme 'catppuccin
      catppuccin-flavor 'macchiato)

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(setq display-line-numbers-type t)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(setq bookmark-default-file "~/.doom/bookmarks")

;; Keybindings
(map! :leader
      :desc "Toggle comment lines" "TAB TAB" #'comment-line
      :desc "Toggle fold code block" "C-." #'+fold/toggle
      (:prefix ("t" . "toggle")
               :desc "Toggle treemacs" "t" #'+treemacs/toggle
               :desc "Toggle vterm" "j" #'+vterm/toggle)
      (:prefix ("b" . "buffer")
               :desc "List bookmarks" "L" #'list-bookmarks
               :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

;; Treemacs config
(after! treemacs
  (setq +treemacs-git-mode 'deferred)
  (treemacs-follow-mode 1))

;; Wakatime config
(after! wakatime-mode
  (global-wakatime-mode 1))

;; Langs
;; Elixir config
(setq lsp-elixir-fetch-deps t
      lsp-elixir-suggest-specs nil)

;; Org
(after! org
  (setq org-agenda-files '("~/org/agenda.org")
        org-journal-dir "~/org/journal"
        org-journal-date-prefix "* "
        org-journal-time-prefix "** "
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org"
        org-roam-directory "~/org/roam"
        org-roam-graph-viewer "/etc/profiles/per-user/douglas/bin/firefox"))
