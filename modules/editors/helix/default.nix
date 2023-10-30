{
  custom,
  lib,
  next-ls,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf;
  cfg = custom.helix.languages;
  lexical-lsp = pkgs.beam.packages.erlangR26.callPackage ../../shells/lexical-lsp.nix {};
  vscode-lsp = pkgs.nodePackages.vscode-langservers-extracted;
in {
  options.helix = {
    enable = mkEnableOption "Enables Helix editor";
    languages = {
      css.enable = mkEnableOption "Enables CSS support";
      elixir.enable = mkEnableOption "Enables Elixir support";
      html.enable = mkEnableOption "Enables HTML support";
      json.enable = mkEnableOption "Enables JSON support";
      nix.enable = mkEnableOption "Enables Nix support";
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
            left = ["mode" "spinner" "spacer" "version-control"];
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
            #C-right = "move_next_word_start";
            #C-left = "move_prev_word_end";

            #C-A-up = ["ensure_selections_forward" "extend_to_line_bounds" "extend_char_right" "extend_char_left" "delete_selection" "move_line_up" "add_newline_above" "move_line_up" "replace_with_yanked"];
            #C-A-down = ["ensure_selections_forward" "extend_to_line_bounds" "extend_char_right" "extend_char_left" "delete_selection" "add_newline_below" "move_line_down" "replace_with_yanked"];
          };
          #insert = {
          #C-right = "move_next_word_start";
          #C-left = "move_prev_word_end";
          #};
        };
      };

      languages = {
        language-server = {
          nextls = mkIf cfg.elixir.enable {
            command = "${next-ls.packages."${pkgs.system}".default}/bin/nextls";
            args = ["--stdio=true"];
          };
          nil.command = mkIf cfg.nix.enable "${pkgs.nil}/bin/nil";
          lexical-lsp.command = mkIf cfg.elixir.enable "${lexical-lsp}/bin/lexical";
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
          mix = {
            formatter = {
              command = "mix";
              args = ["format" "-"];
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
          (mkIf cfg.nix.enable {
            name = "nix";
            auto-format = true;
            formatter.command = "${pkgs.alejandra}/bin/alejandra";
          })
        ];
      };
    };
  };
}
