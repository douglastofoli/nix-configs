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

  -- Theme
import Colors.Dracula

myFont :: String
myFont = "xft:RobotoMono Nerd Font:regular:size=11:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrownser :: String
myBrownser = "firefox"

myEditor :: String
myEditor = "code"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor :: String
myNormalBorderColor = colorBack

myFocusedBorderColor :: String
myFocusedBorderColor = color15

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
  spawn "killall trayer"

  spawnOnce "telegram-desktop"

  spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 -transparent true --alpha 0 " ++ colorTrayer ++ " --height 22")

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where navKeyMap = M.fromList 
          [ ((0, xK_Escape), cancel)
          , ((0, xK_Return), select)
          , ((0, xK_slash), substringSearch myNavigation)
          , ((0, xK_Left), move (-1, 0) >> myNavigation)
          , ((0, xK_h), move (-1, 0) >> myNavigation)
          , ((0, xK_Right), move (1, 0) >> myNavigation)
          , ((0, xK_l), move (1, 0) >> myNavigation)
          , ((0, xK_Down), move (0, 1) >> myNavigation)
          , ((0, xK_j), move (0, 1) >> myNavigation)
          , ((0, xK_Up), move (0, -1) >> myNavigation)
          , ((0, xK_k), move (0, -1) >> myNavigation)
          , ((0, xK_y), move (-1, -1) >> myNavigation)
          , ((0, xK_i), move (1, -1) >> myNavigation)
          , ((0, xK_n), move (-1, 1) >> myNavigation)
          , ((0, xK_m), move (1, -1) >> myNavigation)
          , ((0, xK_space), setPos (0, 0) >> myNavigation)
          ]
        navDefaultHandler = const myNavigation

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
  (0x28, 0x2c, 0x34) -- lowest inactive bg
  (0x28, 0x2c, 0x34) -- highest inactive bg
  (0xc7, 0x92, 0xea) -- active bg
  (0xc0, 0xa7, 0x9a) -- inactive bg
  (0x28, 0x2c, 0x34) -- active fg

myGridConfig :: p -> GSConfig Window
myGridConfig colorizer = (buildDefaultGSConfig myColorizer)
  { gs_cellheight    = 40
  , gs_cellwidth     = 200
  , gs_cellpadding   = 6
  , gs_navigate      = myNavigation
  , gs_originFractX  = 0.5
  , gs_originFractY  = 0.5
  , gs_font          = myFont
  }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where conf = def
                  { gs_cellheight   = 40
                  , gs_cellwidth    = 180
                  , gs_cellpadding  = 6
                  , gs_originFractX = 0.5
                  , gs_originFractY = 0.5
                  , gs_font         = myFont
                  }
    
runSelectedAction' :: GSConfig (X ()) -> [(String, X ())] -> X ()
runSelectedAction' conf actions = do
  selectedActionM <- gridselect conf actions
  case selectedActionM of
    Just selectedAction -> selectedAction
    Nothing -> return ()

gsCategories =
  [ ("Games",      "xdotool key super+alt+1")
  , ("Education",  "xdotool key super+alt+2")
  , ("Internet",   "xdotool key super+alt+3")
  , ("Multimedia", "xdotool key super+alt+4")
  , ("Office",     "xdotool key super+alt+5")
  , ("Settings",   "xdotool key super+alt+6")
  , ("System",     "xdotool key super+alt+7")
  , ("Utilities",  "xdotool key super+alt+8")
  ]

gsGames = 
  [ ("Steam", "steam") ]

gsEducation = []

gsInternet = 
  [ ("Discord", "discord")
  , ("Firefox", "firefox")
  , ("Zoom", "zoom")
  ]

gsMultimedia =
  [ ("OBS Stdudio", "obs")
  , ("VLC", "vlc")
  ]

gsOffice = []

gsSettings = []

gsSystem = 
  [ ("Alacritty", "alacritty")
  , ("Bash", "bash")
  , ("PCManFM", "pcmanfm")
  , ("VirtualBox", "virtualbox")
  , ("Zsh", (myTerminal ++ " -e zsh"))
  ]

gsUtilities = 
  [ ("Emacs", "emacs")
  , ("Visual Studio Code", "code")
  , ("Vim", (myTerminal ++ " -e vim"))
  ]

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                -- , NS "calculator" spawnCalc findCalc manageCalc
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w

-- Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable
-- amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Bolow is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall          = renamed [Replace "tall"]
                $ limitWindows 5
                $ smartBorders
                $ windowNavigation
                $ addTabs shrinkText myTabTheme
                $ subLayout [] (smartBorders Simplest)
                $ mySpacing 8
                $ ResizableTall 1 (3/100) (1/2) []
monocle       = renamed [Replace "monocle"]
                $ smartBorders
                $ windowNavigation
                $ addTabs shrinkText myTabTheme
                $ subLayout [] (smartBorders Simplest)
                $ Full
floats        = renamed [Replace "floats"]
                $ smartBorders
                $ simplestFloat
grid          = renamed [Replace "grid"]
                $ limitWindows 9
                $ smartBorders
                $ windowNavigation
                $ addTabs shrinkText myTabTheme
                $ subLayout [] (smartBorders Simplest)
                $ mySpacing 8
                $ mkToggle (single MIRROR)
                $ Grid (16/10)
spirals       = renamed [Replace "spirals"]
                $ limitWindows 9
                $ smartBorders
                $ windowNavigation
                $ addTabs shrinkText myTabTheme
                $ subLayout [] (smartBorders Simplest)
                $ mySpacing' 8
                $ spiral (6/7)
threeCol      = renamed [Replace "threeCol"]
                $ limitWindows 7
                $ smartBorders
                $ windowNavigation
                $ addTabs shrinkText myTabTheme
                $ subLayout [] (smartBorders Simplest)
                $ ThreeCol 1 (3/100) (1/2)
threeRow      = renamed [Replace "threeRow"]
                $ limitWindows 7
                $ smartBorders
                $ windowNavigation
                $ addTabs shrinkText myTabTheme
                $ subLayout [] (smartBorders Simplest)
                $ Mirror
                $ ThreeCol 1 (3/100) (1/2)
tabs          = renamed [Replace "tabs"]
                $ tabbed shrinkText myTabTheme
tallAccordion = renamed [Replace "tallAccordion"]
                $ Accordion
wideAccordion = renamed [Replace "wideAccordion"]
                $ Mirror Accordion

myTabTheme = def 
  {
    fontName = myFont
  , activeColor = color15
  , inactiveColor = color08
  , activeBorderColor = color15
  , inactiveBorderColor = colorBack
  , activeTextColor = colorBack
  , inactiveTextColor = color16
  }

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  {
    swn_font = "xft:RobotoMono Nerd Font:bold:size=35"
  , swn_fade = 1.0
  , swn_bgcolor = "#1c1f24"
  , swn_color = "#ffffff"
  }

myLayoutHook = avoidStruts 
               $ mouseResize 
               $ windowArrange 
               $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = withBorder myBorderWidth tall
                                              ||| noBorders monocle
                                              ||| floats
                                              ||| noBorders tabs
                                              ||| grid
                                              ||| spirals
                                              ||| threeCol
                                              ||| threeRow
                                              ||| tallAccordion
                                              ||| wideAccordion

myWorkspaces :: [String]
myWorkspaces = [" www ", " dev ", " sys ", " chat ", " disc ", " mus ", " vbox ", " vid ", " gfx "]
myWorkspacesIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
  where i = fromJust $ M.lookup ws myWorkspacesIndices

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [ className =? "confirm"          --> doFloat
  , className =? "file_progress"    --> doFloat
  , className =? "dialog"           --> doFloat
  , className =? "download"         --> doFloat
  , className =? "error"            --> doFloat
  , className =? "Gimp"             --> doFloat
  , className =? "notification"     --> doFloat
  , className =? "pinentry-gtk-2"   --> doFloat
  , className =? "splash"           --> doFloat
  , className =? "toolbar"          --> doFloat
  , className =? "telegram-desktop" --> doFloat
  , className =? "Yad"              --> doCenterFloat
  , title =? "Mozilla Firefox"      --> doShift (myWorkspaces !! 1)
  , className =? "Gimp"             --> doShift (myWorkspaces !! 8)
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  , isFullscreen                    --> doFullFloat
  ] <+> namedScratchpadManageHook myScratchPads

subtitle' :: String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0,0), NamedAction $ map toUpper $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where
    sep = replicate (6 + length x) '-'

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ "yad --text-info --fontname=\"RobotoMono Nerd Font 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
  hPutStr h (unlines $ showKm x)
  hClose h
  return ()

myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
  let subKeys str ks = subtitle' str : mkNamedKeymap c ks in
  subKeys "XMonad Essentials"
  [ ("M-C-r", addName "Recompile XMonad"       $ spawn "xmonad --recompile")
  , ("M-S-r", addName "Restart XMonad"         $ spawn "xmonad --restart")
  , ("M-S-q", addName "Quit XMonad"            $ io exitSuccess)
  , ("M-S-c", addName "Kill focused window"    $ kill1)
  , ("M-S-a", addName "Kill all windows on WS" $ killAll)
  , ("M-S-<Return>", addName "Run rofi prompt" $ spawn "rofi -dmenu -p 'Run command:'")
  , ("M-d", addName "Run rofi apps"            $ spawn "rofi -no-lazy-grab -show drun")
  ]

  ^++^ subKeys "Switch to workspace"
  [ ("M-1", addName "Switch to workspace 1"  $ (windows $ W.greedyView $ myWorkspaces !! 0))
  , ("M-2", addName "Switch to workspace 2"  $ (windows $ W.greedyView $ myWorkspaces !! 1))
  , ("M-3", addName "Switch to workspace 3"  $ (windows $ W.greedyView $ myWorkspaces !! 2))
  , ("M-4", addName "Switch to workspace 4"  $ (windows $ W.greedyView $ myWorkspaces !! 3))
  , ("M-5", addName "Switch to workspace 5"  $ (windows $ W.greedyView $ myWorkspaces !! 4))
  , ("M-6", addName "Switch to workspace 6"  $ (windows $ W.greedyView $ myWorkspaces !! 5))
  , ("M-7", addName "Switch to workspace 7"  $ (windows $ W.greedyView $ myWorkspaces !! 6))
  , ("M-8", addName "Switch to workspace 8"  $ (windows $ W.greedyView $ myWorkspaces !! 7))
  , ("M-9", addName "Switch to workspace 9"  $ (windows $ W.greedyView $ myWorkspaces !! 8))
  ]

   ^++^ subKeys "Send window to workspace"
  [ ("M-S-1", addName "Send window to workspace 1"  $ (windows $ W.shift $ myWorkspaces !! 0))
  , ("M-S-2", addName "Send window to workspace 2"  $ (windows $ W.shift $ myWorkspaces !! 1))
  , ("M-S-3", addName "Send window to workspace 3"  $ (windows $ W.shift $ myWorkspaces !! 2))
  , ("M-S-4", addName "Send window to workspace 4"  $ (windows $ W.shift $ myWorkspaces !! 3))
  , ("M-S-5", addName "Send window to workspace 5"  $ (windows $ W.shift $ myWorkspaces !! 4))
  , ("M-S-6", addName "Send window to workspace 6"  $ (windows $ W.shift $ myWorkspaces !! 5))
  , ("M-S-7", addName "Send window to workspace 7"  $ (windows $ W.shift $ myWorkspaces !! 6))
  , ("M-S-8", addName "Send window to workspace 8"  $ (windows $ W.shift $ myWorkspaces !! 7))
  , ("M-S-9", addName "Send window to workspace 9"  $ (windows $ W.shift $ myWorkspaces !! 8))
  ]

  ^++^ subKeys "Move window to WS and go there"
  [ ("M-S-<Page_Up>", addName "Move window to next WS"   $ shiftTo Next nonNSP >> moveTo Next nonNSP)
  , ("M-S-<Page_Down>", addName "Move window to prev WS" $ shiftTo Prev nonNSP >> moveTo Prev nonNSP)
  ]

  ^++^ subKeys "Window navigation"
  [ ("M-j", addName "Move focus to next window"               $ windows W.focusDown)
  , ("M-k", addName "Move focus to prev window"               $ windows W.focusUp)
  , ("M-m", addName "Move focus to master window"             $ windows W.focusMaster)
  , ("M-S-j", addName "Swap focused window with next window"  $ windows W.swapDown)
  , ("M-S-k", addName "Swap focused window with prev window"  $ windows W.swapUp)
  , ("M-S-m", addName "Swap focused window with prev window"  $ windows W.swapMaster)
  , ("M-<Backspace>", addName "Move focused window to master" $ promote)
  , ("M-S-,", addName "Rotate all windows except master"      $ rotSlavesDown)
  , ("M-S-.", addName "Rotate all windows current stack"      $ rotAllDown)
  ]

  ^++^ subKeys "Switch layouts"
  [ ("M-<Tab>", addName "Switch to next layout"     $ sendMessage NextLayout)
  , ("M-<Space>", addName "Toggle noborders/full"   $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
  ]

  ^++^ subKeys "Favorite programs"
  [ ("M-<Return>", addName "Lauch terminal"         $ spawn (myTerminal))
  , ("M-b", addName "Lauch web brownser"           $ spawn (myBrownser))
  ]

  ^++^ subKeys "Monitors"
  [ ("M-.", addName "Switch focus to next monitor" $ nextScreen)
  , ("M-,", addName "Switch focus to prev monitor" $ prevScreen)
  ]

  ^++^ subKeys "Switch layouts"
  [ ("M-<Tab>", addName "Switch to next layout"   $ sendMessage NextLayout)
  , ("M-<Space>", addName "Toggle noborders/full" $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
  ]

  ^++^ subKeys "Window resizing"
  [ ("M-h", addName "Shrink window"               $ sendMessage Shrink)
  , ("M-l", addName "Expand window"               $ sendMessage Expand)
  , ("M-M1-j", addName "Shrink window vertically" $ sendMessage MirrorShrink)
  , ("M-M1-k", addName "Expand window vertically" $ sendMessage MirrorExpand)
  ]

  ^++^ subKeys "Floating windows"
  [ ("M-f", addName "Toggle float layout"        $ sendMessage (T.Toggle "floats"))
  , ("M-t", addName "Sink a floating window"     $ withFocused $ windows . W.sink)
  , ("M-S-t", addName "Sink all floated windows" $ sinkAll)
  ]

  ^++^ subKeys "Scratchpads"
  [ ("M-s t", addName "Toggle scratchpad terminal"   $ namedScratchpadAction myScratchPads "terminal")
  --, ("M-s m", addName "Toggle scratchpad mocp"       $ namedScratchpadAction myScratchPads "mocp")
  --, ("M-s c", addName "Toggle scratchpad calculator" $ namedScratchpadAction myScratchPads "calculator")
  ]

  ^++^ subKeys "GridSelect"
  [ ("M-M1-<Return>", addName "Select favorite apps" $ spawnSelected'
       $ gsGames ++ gsEducation ++ gsInternet ++ gsMultimedia ++ gsOffice ++ gsSettings ++ gsSystem ++ gsUtilities)
  , ("M-M1-c", addName "Select favorite apps"    $ spawnSelected' gsCategories)
  , ("M-M1-t", addName "Goto selected window"    $ goToSelected $ myGridConfig myColorizer)
  , ("M-M1-b", addName "Bring selected window"   $ bringSelected $ myGridConfig myColorizer)
  , ("M-M1-1", addName "Menu of games"           $ spawnSelected' gsGames)
  , ("M-M1-2", addName "Menu of education apps"  $ spawnSelected' gsEducation)
  , ("M-M1-3", addName "Menu of Internet apps"   $ spawnSelected' gsInternet)
  , ("M-M1-4", addName "Menu of multimedia apps" $ spawnSelected' gsMultimedia)
  , ("M-M1-5", addName "Menu of office apps"     $ spawnSelected' gsOffice)
  , ("M-M1-6", addName "Menu of settings apps"   $ spawnSelected' gsSettings)
  , ("M-M1-7", addName "Menu of system apps"     $ spawnSelected' gsSystem)
  , ("M-M1-8", addName "Menu of utilities apps"  $ spawnSelected' gsUtilities)
  ]

  ^++^ subKeys "Multimedia keys"
  [ ("<XF86AudioMute>", addName "Toggle audio mute"   $ spawn "amixer set Master toggle")
  , ("<XF86AudioLowerVolume>", addName "Lower vol"    $ spawn "amixer set Master 5%- unmute")
  , ("<XF86AudioRaiseVolume>", addName "Raise vol"    $ spawn "amixer set Master 5%+ unmute")
  , ("M-<Print>", addName "Take screenshot"           $ spawn "flameshot gui -p $HOME/gdrive/Screenshots")
  ]

  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))
    nonEmptyNonNSP = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

main :: IO ()
main = do
  xmproc0 <- spawnPipe ("xmobar -x 0")
  xmonad $ addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) myKeys $ ewmh $ docks $ def
    { manageHook         = myManageHook <+> manageDocks
    , handleEventHook    = swallowEventHook (className =? "Alacritty" <||> className =? "st-256color" <||> className =? "XTerm") (return True)
    , modMask            = myModMask 
    , terminal           = myTerminal
    , startupHook        = myStartupHook
    , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , logHook            = dynamicLogWithPP $ filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
        { ppOutput = \x -> hPutStrLn xmproc0 x
        , ppCurrent = xmobarColor color06 "" . wrap ("<box type=Bottom width=2 mb=2 color=" ++ color06 ++ ">") "</box>"
        , ppVisible = xmobarColor color06 "" . clickable
        , ppHidden = xmobarColor color05 "" . wrap ("<box type=Top width=2 mt=2 color=" ++ color05 ++ ">") "</box>" . clickable 
        , ppHiddenNoWindows = xmobarColor color05 "" . clickable
        , ppTitle = xmobarColor color16 "" . shorten 60
        , ppSep = "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>"
        , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
        , ppExtras = [windowCount]
        , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        }
    }
