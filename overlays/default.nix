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
  ];
}
