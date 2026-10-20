{
  flake.modules.nixos.desktop =
    { config, ... }:
    {
      networking = {
        hostName = config.host.name;

        useDHCP = false;

        firewall.allowedTCPPorts = [
          5900
          8081
          19000
          19001
          19002
        ];

        networkmanager.enable = true;

        nameservers = [
          "45.90.28.0"
          "45.90.30.0"
          "2a07:a8c0::"
          "2a07:a8c1::"
        ];
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
