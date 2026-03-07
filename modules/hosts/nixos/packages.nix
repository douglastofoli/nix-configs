{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      environment = {
        # variables = {
        #   TERMINAL = "${vars.terminal}";
        #   EDITOR = "${vars.editor}";
        #   VISUAL = "${vars.editor}";
        # };
        systemPackages = with pkgs; [
          # Terminal
          bat
          btop # Resource Manager
          coreutils # GNU Utilities
          gnupg
          eza
          jq # JSON Manipulation
          killall # Process Killer
          libnotify
          nano # Text Editor
          nodejs # Javascript Runtime
          nodePackages.npm # Package Manager
          pciutils # Manage PCI
          ranger # File Manager
          ripgrep # Recursive Search
          smartmontools # Disk Health
          tldr # Helper
          trash-cli # Recycle Bin
          usbutils # Manage USB
          wget # Retriever
          xdg-utils # Environment integration
          zoxide

          # Video/Audio
          feh # Image Viewer
          grim # Screenshot
          swappy # Screenshot Editor
          slurp # Screenshot Selector

          # Apps
          appimage-run # Runs AppImages on NixOS
          firefox # Browser
          google-chrome # Browser
          cura-appimage

          # File Management
          file-roller # Archive Manager
          thunar # File Browser
          p7zip # Zip Encryption
          rsync # Syncer - $ rsync -r dir1/ dir2/
          unzip # Zip Files
          unrar # Rar Files
          zip # Zip
          stable.image-roll # Image viewer
        ];
      };
    };
}
