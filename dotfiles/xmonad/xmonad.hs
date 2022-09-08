  -- Base
import XMonad
import XMonad.Layout.Spacing
import System.IO (hClose, hPutStr, hPutStrLn)
import System.Exit
import qualified XMonad.StackSet as W

  -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

  -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust, isJust)
import Data.Monoid
import Data.Tree
import qualified Data.Map as M

  -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.Minimize
import XMonad.Hooks.InsertPosition

  -- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

  -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

  -- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers
import XMonad.Util.Ungrab

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
      . withEasySB (statusBarProp "xmobar" (pure C.myXmobarPP)) defToggleStrutsKey
      $ myConfig
    