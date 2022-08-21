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
      insync-v3 = super.insync-v3.overrideAttrs (_: {
        src = builtins.fetchurl {
          url = "https://cdn.insynchq.com/builds/linux/insync_3.7.11.50381-jammy_amd64.deb";
          sha256 = "12516y39zdvz2l5m0d49fvnd9jbq9wfpvrhnmbxajzfar6b69yk4";
        };
        installPhase = ''
          mkdir -p $out/bin $out/lib $out/share
          cp -R usr/* $out/

          rm -f $out/lib/insync/libGLX.so.0

          sed -i 's|/usr/lib/insync|/lib/insync|' "$out/bin/insync"
          wrapQtApp "$out/lib/insync/insync"
        '';
      });
    })
    (self: super: {
      picom = super.picom.overrideAttrs (_: {      
        src = super.fetchFromGitHub {
          repo = "picom";
          owner = "jonaburg";
          rev = "e3c19cd7d1108d114552267f302548c113278d45";
          sha256 = "4voCAYd0fzJHQjJo4x3RoWz5l3JJbRvgIXn1Kg6nz6Y";
        };
      });
    })
  ];
}
