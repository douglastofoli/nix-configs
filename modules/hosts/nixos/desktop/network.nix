{
  flake.modules.nixos.desktop =
    { config, lib, ... }:
    {
      networking = {
        useDHCP = false;
        hostName = config.host.name;
        networkmanager.enable = true;
        networkmanager.dns = "systemd-resolved";

        interfaces.enp0s3 = {
          ipv4.addresses = [
            {
              address = "192.168.3.20";
              prefixLength = 24;
            }
          ];
        };

        defaultGateway = "192.168.3.1";
      };

      services.resolved = {
        enable = true;
        settings = {
          Resolve = {
            DNS = [
              "45.90.28.0#91823f.dns.nextdns.io"
              "2a07:a8c0::#91823f.dns.nextdns.io"
              "45.90.30.0#91823f.dns.nextdns.io"
              "2a07:a8c1::#91823f.dns.nextdns.io"
            ];

            FallbackDNS = [
              "45.90.28.0#91823f.dns.nextdns.io"
              "2a07:a8c0::#91823f.dns.nextdns.io"
              "45.90.30.0#91823f.dns.nextdns.io"
              "2a07:a8c1::#91823f.dns.nextdns.io"
            ];

            DNSOverTLS = true;
          };
        };
      };
    };
}
