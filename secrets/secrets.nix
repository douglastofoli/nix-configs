let
  user1 =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE6wwvEyfEnXinoT/OIXxe+ESDZeKTDM93iAHy1cBDmQ";
  users = [ user1 ];

  system1 =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE6wwvEyfEnXinoT/OIXxe+ESDZeKTDM93iAHy1cBDmQ";
  systems = [ system1 ];
in {
  "userpassword.age".publicKeys = [ user1 system1 ];
  "gitsigningkey.age".publicKeys = [ user1 system1 ];
}
