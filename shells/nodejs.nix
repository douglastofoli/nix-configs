{
  pkgs,
  ...
}:

{
  packages = with pkgs; [
    nodejs
    pnpm
  ];

  shellHook = ''
    export PNPM_HOME="$HOME/.local/share/pnpm"
    export PATH="$PNPM_HOME:$PATH"

    echo "pnpm ready"
  '';
}
