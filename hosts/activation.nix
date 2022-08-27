{ config, location, ... }:

{
  system.userActivationScripts = {
    source = {
      text = ''
        source ${config.system.build.setEnvironment}
      '';
    };

    xmobar = {
      text = ''
        CONFIG="$HOME/.config"

        if [ ! -d "$CONFIG/xmobar" ]; then
          ln -s ${location}/dotfiles/xmobar $CONFIG/xmobar
        fi
      '';
    };
  };
}
