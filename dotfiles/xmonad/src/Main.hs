module Main where
  
import XMonad
import System.IO (hClose, hPutStr, hPutStrLn)

import XMonad.Hooks.InsertPosition
import XMonad.Hooks.Minimize
import XMonad.Hooks.ServerMode

import qualified XMonad.Custom.ManageHook as C
import qualified XMonad.Custom.Variables as C
import qualified XMonad.Custom.Colors.Dracula as C
import qualified XMonad.Custom.StartupHook as C
import qualified XMonad.Custom.Layouts as C
import qualified XMonad.Custom.Workspaces as C
import qualified XMonad.Custom.Keys as C

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  {
    swn_font = "xft:RobotoMono Nerd Font:bold:size=35"
  , swn_fade = 1.0
  , swn_bgcolor = "#1c1f24"
  , swn_color = "#ffffff"
  }

main :: IO ()
main do
  xmproc <- spawnPipe ("xmobar")
  xmonad $ addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) myKeys $ ewmh $ docks $ def 
    { modMask = C.myModMask
    , terminal = C.myTerminal
    , manageHook = insertPosition End Newer <+> C.myManageHook
    , handleEventHook = serverModeEventHookCmd
                        <+> serverModeEventHook
                        <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                        <+> minimizeEventHook
                        <+> swallowEventHook (className =? "Alacritty" <||> className =? "st-256color" <||> className =? "XTerm") (return True)
    , startupHook = C.myStartupHook
    , layoutHook = showWName' myShowWNameTheme $ C.myLayoutHook
    }
  