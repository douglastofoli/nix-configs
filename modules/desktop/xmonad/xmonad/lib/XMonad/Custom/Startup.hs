module XMonad.Custom.Startup (myStartupHook) where

import XMonad
import XMonad.Custom.Colors.Dracula qualified as C
import XMonad.Hooks.SetWMName
import XMonad.Util.SpawnOnce

myStartupHook :: X ()
myStartupHook = do
  spawn "killall trayer"

  spawnOnce "feh -zr --bg-fill --no-fehbg $HOME/.config/wallpaper.jpg"
  spawnOnce "google-drive-ocamlfuse -label google-drive $HOME/GoogleDrive"

  spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 " ++ C.colorTrayer ++ " --height 30")

  setWMName "LG3D"
