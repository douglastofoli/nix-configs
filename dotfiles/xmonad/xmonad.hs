  -- Base
import XMonad
import System.IO (hPutStrLn)
  -- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks 
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.Minimize
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))

  -- Data
import Data.Maybe (fromJust)
import qualified Data.Map as M

  -- Utilities
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)

  -- Theme
import XMonad.Custom.Colors.Dracula as C

  -- Custom
import qualified XMonad.Custom.Keys as C
import qualified XMonad.Custom.ManageHook as C
import qualified XMonad.Custom.LayoutHook as C
import qualified XMonad.Custom.StartupHook as C
import qualified XMonad.Custom.Variables as C
import qualified XMonad.Custom.Workspaces as C
import qualified XMonad.Custom.XMobar as C

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
      where i = fromJust $ M.lookup ws C.myWorkspaceIndices

main :: IO ()
main = do
  -- Launching three instances of xmobar on their monitors.
  xmproc0 <- spawnPipe ("xmobar -x 0 $HOME/.config/xmobar/xmobarrc")
  -- the xmonad, ya know...what the WM is named after!
  xmonad $ addDescrKeys' ((mod4Mask, xK_F1), C.showKeybindings) C.myKeys $ ewmhFullscreen $ ewmh $ docks $ def
    { manageHook         = C.myManageHook <+> manageDocks
    , handleEventHook    = swallowEventHook (className =? "Alacritty"  <||> className =? "st-256color" <||> className =? "XTerm") (return True)
    , modMask            = C.myModMask
    , terminal           = C.myTerminal
    , startupHook        = C.myStartupHook
    , layoutHook         = C.myLayoutHook
    , workspaces         = C.myWorkspaces
    , borderWidth        = C.myBorderWidth
    , normalBorderColor  = C.myNormalBorderColor
    , focusedBorderColor = C.myFocusedBorderColor
    , logHook = dynamicLogWithPP $ filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
        { ppOutput = \x -> hPutStrLn xmproc0 x   -- xmobar on monitor 1
                       -- >> hPutStrLn xmproc1 x   -- xmobar on monitor 2
                       -- >> hPutStrLn xmproc2 x   -- xmobar on monitor 3
        , ppCurrent = xmobarColor C.color06 "" . wrap
                      ("<box type=Bottom width=2 mb=2 color=" ++ C.color06 ++ ">") "</box>"
          -- Visible but not current workspace
        , ppVisible = xmobarColor C.color06 "" . clickable
          -- Hidden workspace
        , ppHidden = xmobarColor C.color05 "" . wrap
                     ("<box type=Top width=2 mt=2 color=" ++ C.color05 ++ ">") "</box>" . clickable
          -- Hidden workspaces (no windows)
        , ppHiddenNoWindows = xmobarColor C.color05 ""  . clickable
          -- Title of active window
        , ppTitle = xmobarColor C.color16 "" . shorten 60
          -- Separator character
        , ppSep =  "<fc=" ++ C.color09 ++ "> <fn=1>|</fn> </fc>"
          -- Urgent workspace
        , ppUrgent = xmobarColor C.color02 "" . wrap "!" "!"
          -- Adding # of windows on current workspace to the bar
        , ppExtras  = [C.windowCount]
          -- order of things in xmobar
        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        }
    }
