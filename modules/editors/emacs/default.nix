{ config, location, pkgs, ... }:

{
  services.emacs = {
    enable = true;
    package = pkgs.emacs28NativeComp;
  };

  system.userActivationScripts = {
    doomEmacs = {
      text = ''
        source ${config.system.build.setEnvironment}
        EMACS="$HOME/.emacs.d"

        if [ ! -d "$EMACS" ]; then
          ln -s ${location}/modules/editors/emacs/doom.d $HOME/.doom.d

          git clone --depth 1 https://github.com/doomemacs/doomemacs $EMACS
          yes | $EMACS/bin/doom install

          $EMACS/bin/doom sync
        else
          $EMACS/bin/doom sync
        fi
      '';
    };
  };
}
