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
      insync-v3 = super.insync-v3.overrideAttrs (_: {
        version = "3.7.11.50381";
        src = super.fetchurl {
          url =
            "https://cdn.insynchq.com/builds/linux/insync_3.7.11.50381-focal_amd64.deb";
          sha256 = "sha256-W4YUjQ8VdU+m5DwPlokO0i/mKWOi/1vN79ZmMJk9dZM=";
        };
      });
    })
  ];
}
