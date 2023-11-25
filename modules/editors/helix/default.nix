{
  custom-config,
  lexical-lsp,
  lib,
  next-ls,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf types;
  cfg = custom-config.helix;
  languages = cfg.languages;
  typescript-lsp = pkgs.nodePackages.typescript-language-server;
  vscode-lsp = pkgs.nodePackages.vscode-langservers-extracted;
in {
  options.helix = {
    enable = mkEnableOption {
      description = "Enables Helix editor";
      type = types.bool;
      default = false;
    };
    languages = {
      clojure.enable = mkEnableOption {
        description = "Enables Clojure support";
        type = types.bool;
        default = false;
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
        default = true;
      };
      nix.enable = mkEnableOption {
        description = "Enables Nix support";
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    programs.helix = {
      inherit (cfg) enable;

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
          clojure-lsp.command = mkIf languages.clojure.enable "${pkgs.clojure-lsp}/bin/clojure-lsp";
          haskell-language-server = mkIf languages.haskell.enable {
            command = "${pkgs.haskell-language-server}/bin/haskell-language-server-wrapper";
            args = ["--lsp"];
          };
          lexical-lsp.command = mkIf languages.elixir.enable "${lexical-lsp.packages."${pkgs.system}".default}/bin/lexical";
          marksman.command = mkIf languages.markdown.enable "${pkgs.marksman}/bin/marksman";
          nextls = mkIf languages.elixir.enable {
            command = "${next-ls.packages."${pkgs.system}".default}/bin/nextls";
            args = ["--stdio=true"];
          };
          nil.command = mkIf languages.nix.enable "${pkgs.nil}/bin/nil";
          scss = mkIf languages.css.enable {
            command = "${vscode-lsp}/bin/vscode-css-language-server";
            args = ["--stdio"];
            config = {
              provideFormatter = true;
              scss = {validate.enable = true;};
            };
          };
          typescript-language-server = mkIf languages.javascript.enable {
            command = "${typescript-lsp}/bin/typescript-language-server";
            args = ["--stdio"];
          };
          vscode-css-language-server = mkIf languages.css.enable {
            command = "${vscode-lsp}/bin/vscode-css-language-server";
            args = ["--stdio"];
            config = {
              provideFormatter = true;
              css = {validate.enable = true;};
            };
          };
          vscode-html-language-server = mkIf languages.html.enable {
            command = "${vscode-lsp}/bin/vscode-css-language-server";
            args = ["--stdio"];
            config = {
              provideFormatter = true;
              html = {validate.enable = true;};
            };
          };
          vscode-json-language-server = mkIf languages.json.enable {
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
          (mkIf languages.elixir.enable {
            inherit (mix) formatter;
            name = "elixir";
            auto-format = true;
            language-servers = ["lexical-lsp" "nextls"];
          })
          (mkIf languages.elixir.enable {
            inherit (mix) formatter;
            name = "eex";
            auto-format = true;
          })
          (mkIf languages.elixir.enable {
            inherit (mix) formatter;
            name = "heex";
            auto-format = true;
          })
          (mkIf languages.javascript.enable {
            inherit (prettier) formatter;
            name = "javascript";
            auto-format = true;
            language-servers = ["typescript-language-server"];
          })
          (mkIf languages.javascript.enable {
            inherit (prettier) formatter;
            name = "typescript";
            auto-format = true;
            language-servers = ["typescript-language-server"];
          })
          (mkIf languages.javascript.enable {
            inherit (prettier) formatter;
            name = "jsx";
            auto-format = true;
          })
          (mkIf languages.javascript.enable {
            inherit (prettier) formatter;
            name = "tsx";
            auto-format = true;
          })
          (mkIf languages.nix.enable {
            inherit (alejandra) formatter;
            name = "nix";
            auto-format = true;
          })
        ];
      };
    };
  };
}
