{
  nixpkgs.overlays = [ # This overlay will pull the latest version of Discord
    (self: super: {
      discord = super.discord.overrideAttrs (_: {
        src = builtins.fetchTarball {
          url = "https://discord.com/api/download?platform=linux&format=tar.gz";
          sha256 = "1bhjalv1c0yxqdra4gr22r31wirykhng0zglaasrxc41n0sjwx0m";
        };
      });
    })
  ];
}
