{ pkgs, ... }:

{
  programs.vscode = {
    enable = true;

    extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "Nix";
        publisher = "bbenoist";
        version = "1.0.1";
        sha256 = "0zd0n9f5z1f0ckzfjr38xw2zzmcxg1gjrava7yahg5cvdcw6l35b";
      }
      {
        name = "better-toml";
        publisher = "bungcip";
        version = "0.3.2";
        sha256 = "08lhzhrn6p0xwi0hcyp6lj9bvpfj87vr99klzsiy8ji7621dzql3";
      }
      {
        name = "path-intellisense";
        publisher = "christian-kohler";
        version = "2.8.1";
        sha256 = "1j7q4mzj173sl6xl3zjw40hnqvyqsrsczakmv63066k4k0rb6clm";
      }
      {
        name = "theme-dracula";
        publisher = "dracula-theme";
        version = "2.24.2";
        sha256 = "1bsq00h30x60rxhqfdmadps5p1vpbl2kkwgkk6yqs475ic89dnk0";
      }
      {
        name = "EditorConfig";
        publisher = "EditorConfig";
        version = "0.16.4";
        sha256 = "0fa4h9hk1xq6j3zfxvf483sbb4bd17fjl5cdm3rll7z9kaigdqwg";
      }
      {
        name = "elixir-ls";
        publisher = "JakeBecker";
        version = "0.11.0";
        sha256 = "0cryz4jnkyyv0w1rwz62d8c1ki9lakhbcmffmwdbzni67p4g0jx2";
      }
      {
        name = "fluent-icons";
        publisher = "miguelsolorio";
        version = "0.0.18";
        sha256 = "02zrlaq4f29vygisgsyx0nafcccq92mhms420qj0lgshipih0kdh";
      }
      {
        name = "dotenv";
        publisher = "mikestead";
        version = "1.0.1";
        sha256 = "0rs57csczwx6wrs99c442qpf6vllv2fby37f3a9rhwc8sg6849vn";
      }
      {
        name = "vscode-docker";
        publisher = "ms-azuretools";
        version = "1.22.1";
        sha256 = "1ix363fjxi9g450rs3ghx44z3hppvasf0xpzgha93m90djd7ai52";
      }
      {
        name = "color-highlight";
        publisher = "naumovs";
        version = "2.6.0";
        sha256 = "1ssh5d4kn3b57gfw5w99pp3xybdk2xif8z6l7m3y2qf204wd1hsd";
      }
      {
        name = "material-icon-theme";
        publisher = "PKief";
        version = "4.20.0";
        sha256 = "0zb1g1qdn0ws3xnpd7kz4g4drx99m691172b8hpgli37z7zlvw9r";
      }
      {
        name = "vscode-yaml";
        publisher = "redhat";
        version = "1.10.20220805";
        sha256 = "sha256-XFg6L+DYhq7cmbnYPkRaZJK5zPTp4yeQR24fCpUGp4I=";
      }
      {
        name = "errorlens";
        publisher = "usernamehw";
        version = "3.6.0";
        sha256 = "1sv8vlzmynbz20vmv901nrg12wcmsg5i9pm6mqq32rlgb7rw3p50";
      }
      {
        name = "vscode-wakatime";
        publisher = "WakaTime";
        version = "19.0.1";
        sha256 = "sha256-KZaGzb6NO0cVezvAzBFqHpC3o6yjmAog5y9vPsT+bTs=";
      }  
    ];

    userSettings = {
      "window.titleBarStyle" = "custom";
      "window.title" = "\${dirty}\${activeEditorShort} ❯ \${rootName}\${separator}\${appName}";
      "window.menuBarVisibility" = "toggle";

      "workbench.colorTheme" = "Dracula";
      "workbench.productIconTheme" = "fluent-icons";
      "workbench.iconTheme" = "material-icon-theme";

      "terminal.integrated.fontSize" = 14;
      "terminal.integrated.fontFamily" = "JetBrainsMono Nerd Font";
      "terminal.integrated.defaultProfile.linux" = "${pkgs.zsh}/bin/zsh";
   
      "editor.tabSize" = 2;
      "editor.fontSize" = 14;
      "editor.fontFamily" = "JetBrainsMono Nerd Font";
      "editor.lineHeight" = 20;
      "editor.fontLigatures" = true;
      "editor.formatOnSave" = true;
      "editor.renderLineHighlight" = "gutter";
      "editor.rulers" = [
          100
          120
      ];

      "telemetry.telemetryLevel" = "off";

      "explorer.compactFolders" = false;
    };
  };
}