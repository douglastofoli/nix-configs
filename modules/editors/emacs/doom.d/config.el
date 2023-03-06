(setq user-full-name "douglastofoli"
      user-mail-address "tofoli.douglas@hotmail.com")

(setq doom-theme 'catppuccin
      catppuccin-flavor 'mocha)

(after! org
  (setq org-directory "~/org/"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-superstar-itembullet-alist '((?+ . ?➤) (?- . ?✦)) ; changes +/- symbols in item lists
        org-log-done 'time
        org-hide-emphasis-markers t
        org-table-convert-region-max-lines 20000
        org-todo-keywords '((sequence "TODO(t)" "BLOG(b)" "PROJ(p)" "WORK(w)" "|" "DONE(d)" "CANCELLED(c)"))))

(after! org
  (setq org-agenda-files '("~/org/agenda.org")))

(after! org
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-prefix "* "
        org-journal-time-prefix "** "
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org"))

(after! org
  (setq org-roam-directory "~/org/roam"
        org-roam-graph-viewer "/usr/bin/env firefox"))

(setq bookmark-default-file "~/.doom.d/bookmarks")

(setq display-line-numbers-type t)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 15)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 24))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; Key bindings
(map! :leader
      :desc "Comment or uncomment lines" "TAB TAB" #'comment-line
      (:prefix ("s" . "search")
               :desc "Locate file" "f" #'project-find-file)
      (:prefix ("t" . "toggle")
               :desc "Toggle treemacs" "t" #'+treemacs/toggle
               :desc "Toggle vterm" "j" #'+vterm/toggle
               :desc "Toggle line numbers" "l" #'doom/toggle-line-numbers
               :desc "Toggle line highlight in frame" "h" #'hl-line-mode
               :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
               :desc "Toggle truncate lines" "T" #'toggle-truncate-lines)
      (:prefix ("b" . "buffer")
               :desc "List bookmarks" "L" #'list-bookmarks
               :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(map! :desc "Toggle fold code block"
      "C-." #'+fold/toggle)

;; Treemacs config
(after! treemacs
  (setq +treemacs-git-mode 'deferred)
  (treemacs-follow-mode 1))

;; Wakatime config
(after! wakatime-mode
  (global-wakatime-mode 1))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))
