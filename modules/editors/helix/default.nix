{
  custom,
  lib,
  next-ls,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf types;
  cfg = custom.helix.languages;
  lexical-lsp = pkgs.beam.packages.erlangR25.callPackage ../../shells/lexical-lsp.nix {};
  typescript-lsp = pkgs.nodePackages.typescript-language-server;
  vscode-lsp = pkgs.nodePackages.vscode-langservers-extracted;
in {
  options.helix = {
    enable = mkEnableOption "Enables Helix editor";
    languages = {
      clojure.enable = mkEnableOption {
        description = "Enables Clojure support";
        type = types.bool;
        default = true;
      };
      css.enable = mkEnableOption {
        description = "Enables CSS support";
        type = types.bool;
        default = true;
      };
      elixir.enable = mkEnableOption {
        description = "Enables Elixir support";
        type = types.bool;
        default = false;
      };
      haskell.enable = mkEnableOption {
        description = "Enables Haskell support";
        type = types.bool;
        default = false;
      };
      html.enable = mkEnableOption {
        description = "Enables HTML support";
        type = types.bool;
        default = true;
      };
      javascript.enable = mkEnableOption {
        description = "Enables JavaScript support";
        type = types.bool;
        default = false;
      };
      json.enable = mkEnableOption {
        description = "Enables JSON support";
        type = types.bool;
        default = true;
      };
      markdown.enable = mkEnableOption {
        description = "Enables Markdown support";
        type = types.bool;
        default = false;
      };
      nix.enable = mkEnableOption {
        description = "Enables Nix support";
        type = types.bool;
        default = false;
      };
    };
  };

  config = {
    programs.helix = {
      enable = true;

      settings = {
        theme = "dracula";

        editor = {
          cursorline = true;
          color-modes = true;
          line-number = "relative";
          true-color = true;

          statusline = {
            left = ["mode" "spacer" "spinner" "spacer" "version-control"];
            center = ["file-name"];
            right = [
              "diagnostics"
              "selections"
              "position"
              "file-encoding"
              "file-line-ending"
              "file-type"
            ];
          };

          cursor-shape = {
            insert = "bar";
            normal = "block";
            select = "underline";
          };

          indent-guides = {
            render = true;
            rainbow-option = "dim";
          };

          lsp.display-messages = true;

          whitespace.characters = {
            newline = "↴";
            tab = "⇥";
          };
        };

        keys = {
          normal = {
            esc = ["collapse_selection" "keep_primary_selection"];
            C-right = "move_next_word_start";
            C-left = "move_prev_word_end";

            C-A-up = ["ensure_selections_forward" "extend_to_line_bounds" "extend_char_right" "extend_char_left" "delete_selection" "move_line_up" "add_newline_above" "move_line_up" "replace_with_yanked"];
            C-A-down = ["ensure_selections_forward" "extend_to_line_bounds" "extend_char_right" "extend_char_left" "delete_selection" "add_newline_below" "move_line_down" "replace_with_yanked"];
          };
          insert = {
            C-right = "move_next_word_start";
            C-left = "move_prev_word_end";
          };
        };
      };

      languages = {
        language-server = {
          clojure-lsp.command = mkIf cfg.clojure.enable "${pkgs.clojure-lsp}/bin/clojure-lsp";
          haskell-language-server = mkIf cfg.haskell.enable {
            command = "${pkgs.haskell-language-server}/bin/haskell-language-server-wrapper";
            args = ["--lsp"];
          };
          lexical-lsp.command = mkIf cfg.elixir.enable "${lexical-lsp}/bin/lexical";
          marksman.command = mkIf cfg.markdown.enable "${pkgs.marksman}/bin/marksman";
          nextls = mkIf cfg.elixir.enable {
            command = "${next-ls.packages."${pkgs.system}".default}/bin/nextls";
            args = ["--stdio=true"];
          };
          nil.command = mkIf cfg.nix.enable "${pkgs.nil}/bin/nil";
          scss = mkIf cfg.css.enable {
            command = "${vscode-lsp}/bin/vscode-css-language-server";
            args = ["--stdio"];
            config = {
              provideFormatter = true;
              scss = {validate.enable = true;};
            };
          };
          typescript-language-server = mkIf cfg.javascript.enable {
            command = "${typescript-lsp}/bin/typescript-language-server";
            args = ["--stdio"];
          };
          vscode-css-language-server = mkIf cfg.css.enable {
            command = "${vscode-lsp}/bin/vscode-css-language-server";
            args = ["--stdio"];
            config = {
              provideFormatter = true;
              css = {validate.enable = true;};
            };
          };
          vscode-html-language-server = mkIf cfg.html.enable {
            command = "${vscode-lsp}/bin/vscode-css-language-server";
            args = ["--stdio"];
            config = {
              provideFormatter = true;
              html = {validate.enable = true;};
            };
          };
          vscode-json-language-server = mkIf cfg.json.enable {
            command = "${vscode-lsp}/bin/vscode-css-language-server";
            args = ["--stdio"];
            config = {
              provideFormatter = true;
              json = {validate.enable = true;};
            };
          };
        };

        language = let
          alejandra = {
            formatter = {
              command = "${pkgs.alejandra}/bin/alejandra";
            };
          };
          mix = {
            formatter = {
              command = "mix";
              args = ["format" "-"];
            };
          };
          prettier = {
            formatter = {
              command = "prettier";
              args = ["--parser" "typescript"];
            };
          };
        in [
          (mkIf cfg.elixir.enable {
            inherit (mix) formatter;
            name = "elixir";
            auto-format = true;
            language-servers = ["lexical-lsp" "nextls"];
          })
          (mkIf cfg.elixir.enable {
            inherit (mix) formatter;
            name = "eex";
            auto-format = true;
          })
          (mkIf cfg.elixir.enable {
            inherit (mix) formatter;
            name = "heex";
            auto-format = true;
          })
          (mkIf cfg.javascript.enable {
            inherit (prettier) formatter;
            name = "javascript";
            auto-format = true;
            language-servers = ["typescript-language-server"];
          })
          (mkIf cfg.javascript.enable {
            inherit (prettier) formatter;
            name = "typescript";
            auto-format = true;
            language-servers = ["typescript-language-server"];
          })
          (mkIf cfg.javascript.enable {
            inherit (prettier) formatter;
            name = "jsx";
            auto-format = true;
          })
          (mkIf cfg.javascript.enable {
            inherit (prettier) formatter;
            name = "tsx";
            auto-format = true;
          })
          (mkIf cfg.nix.enable {
            inherit (alejandra) formatter;
            name = "nix";
            auto-format = true;
          })
        ];
      };
    };
  };
}
