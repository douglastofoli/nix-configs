{ pkgs, ... }:

{
  nixpkgs.overlays = [ # This overlay will pull the latest version of Discord
    (self: super: {
      discord = super.discord.overrideAttrs (_: {
        src = builtins.fetchTarball {
          url = "https://discord.com/api/download?platform=linux&format=tar.gz";
          sha256 = "1kwqn1xr96kvrlbjd14m304g2finc5f5ljvnklg6fs5k4avrvmn4";
        };
      });
    })
    (self: super: {
      insync-v3 = super.insync-v3.overrideAttrs (old: rec {
        version = "3.7.11.50381";

        buildInputs = old.buildInputs
          ++ [ pkgs.xorg.libxcb pkgs.libxkbcommon pkgs.libdrm ];

        src = builtins.fetchurl {
          url =
            "https://cdn.insynchq.com/builds/linux/insync_3.7.11.50381-focal_amd64.deb";
          sha256 = "14vm7nck0rnnxz6mpzx2cclycbyj1s4rc3rwwjk4yx8m1y6i91jv";
        };

        installPhase = ''
          mkdir -p $out/bin $out/lib $out/share
          cp -R usr/* $out/
          rm $out/lib/insync/libGLX.so.0
          rm $out/lib/insync/libdrm*.so*
          rm $out/lib/insync/libxkbcommon*.so*
          rm $out/lib/insync/libQt5*
          sed -i 's|/usr/lib/insync|/lib/insync|' "$out/bin/insync"
          wrapQtApp "$out/lib/insync/insync"
        '';
      });
    })
  ];
}
