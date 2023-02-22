module XMonad.Custom.Startup (myStartupHook) where

import XMonad
import qualified XMonad.Custom.Colors.Dracula as C
import XMonad.Util.SpawnOnce
import XMonad.Hooks.SetWMName

myStartupHook :: X ()
myStartupHook = do
  spawn "killall trayer"

  spawnOnce "feh -zr --bg-fill --no-fehbg $HOME/.config/wallpaper.jpg"

  spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 " ++ C.colorTrayer ++ " --height 32")

  setWMName "XMonad"
