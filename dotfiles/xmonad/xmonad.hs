  -- Base
import XMonad

  -- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks 
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.Minimize
import XMonad.Hooks.InsertPosition

  -- Utilities
import XMonad.Util.NamedActions

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

myConfig = def
  { manageHook         = insertPosition End Newer <+> C.myManageHook
  , handleEventHook    = serverModeEventHookCmd <+> serverModeEventHook <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn) <+> minimizeEventHook <+> swallowEventHook (className =? "Alacritty" <||> className =? "st-256color" <||> className =? "XTerm") (return True)
  , modMask            = C.myModMask 
  , terminal           = C.myTerminal
  , startupHook        = C.myStartupHook
  , layoutHook         = C.myLayoutHook
  , workspaces         = C.myWorkspaces
  , borderWidth        = C.myBorderWidth
  , normalBorderColor  = C.myNormalBorderColor
  , focusedBorderColor = C.myFocusedBorderColor
  }

main :: IO ()
main = xmonad 
      . addDescrKeys' ((mod4Mask, xK_F1), C.showKeybindings) C.myKeys 
      . ewmhFullscreen 
      . ewmh 
      . docks 
      . withEasySB (statusBarProp "xmobar -x 0 $HOME/.config/xmobar/xmobarrc.hs" (pure C.myXmobarPP)) defToggleStrutsKey
      $ myConfig
