import Data.Char (isSpace, toUpper)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Monoid
import Data.Tree
import Distribution.Backpack.LinkedComponent (dispLinkedComponent)
import System.Directory
import System.Exit (exitSuccess)
import System.IO (hClose, hPutStr, hPutStrLn)
import XMonad
import XMonad.Actions.CopyWindow (copyToAll, kill1)
import XMonad.Actions.CycleWS (Direction1D (..), WSType (..), moveTo, nextScreen, prevScreen, shiftTo)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (decreaseLimit, increaseLimit, limitWindows)
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, single, (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), windowArrange)
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Hacks (trayerPaddingXmobarEventHook, windowedFullscreenFixEventHook)
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

myFont :: String
myFont = "xft:Cantarell:regular:size=11:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "wezterm" -- Sets default terminal

myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = "emacs" -- Sets emacs as editor

myBorderWidth :: Dimension
myBorderWidth = 2 -- Sets border width for windows

myNormColor :: String -- Border color of normal windows
myNormColor = color01

myFocusColor :: String -- Border color of focused windows
myFocusColor = color08

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- colors
color01 = "#282a36" -- background

color02 = "#44475a" -- current line

color03 = "#f8f8f2" -- foreground

color04 = "#6272a4" -- comment

color05 = "#8be9fd" -- cyan

color06 = "#50fa7b" -- green

color07 = "#ffb86c" -- orange

color08 = "#ff79c6" -- pink

color09 = "#bd93f9" -- purple

color10 = "#ff5555" -- red

color11 = "#f1fa8c" -- yellow

colorTrayer :: String
colorTrayer = "--tint 0x282a36"

myStartupHook :: X ()
myStartupHook = do
  spawn "killall trayer"
  spawn "killall .blueman-applet"
  spawn "killall .light-locker-w"

  spawnOnce "feh -zr --bg-fill --no-fehbg $HOME/.config/wallpaper.jpg"
  spawnOnce "blueman-applet"
  
  spawnOnce "sleep 2 && firefox"
  spawnOnce "sleep 2 && discord"
  spawnOnce "sleep 2 && telegram-desktop"

  spawn "sleep 2 && blueman-applet"
  spawn "sleep 2 && light-locker"
  spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 " ++ colorTrayer ++ " --height 30")

  setWMName "LG3D"

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where
    navKeyMap =
      M.fromList
        [ ((0, xK_Escape), cancel),
          ((0, xK_Return), select),
          ((0, xK_slash), substringSearch myNavigation),
          ((0, xK_Left), move (-1, 0) >> myNavigation),
          ((0, xK_h), move (-1, 0) >> myNavigation),
          ((0, xK_Right), move (1, 0) >> myNavigation),
          ((0, xK_l), move (1, 0) >> myNavigation),
          ((0, xK_Down), move (0, 1) >> myNavigation),
          ((0, xK_j), move (0, 1) >> myNavigation),
          ((0, xK_Up), move (0, -1) >> myNavigation),
          ((0, xK_k), move (0, -1) >> myNavigation),
          ((0, xK_y), move (-1, -1) >> myNavigation),
          ((0, xK_i), move (1, -1) >> myNavigation),
          ((0, xK_n), move (-1, 1) >> myNavigation),
          ((0, xK_m), move (1, -1) >> myNavigation),
          ((0, xK_space), setPos (0, 0) >> myNavigation)
        ]
    navDefaultHandler = const myNavigation

myColorizer :: Window -> Bool -> X (String, String)
myColorizer =
  colorRangeFromClassName
    (0x28, 0x2c, 0x34) -- lowest inactive bg
    (0x28, 0x2c, 0x34) -- highest inactive bg
    (0xc7, 0x92, 0xea) -- active bg
    (0xc0, 0xa7, 0x9a) -- inactive fg
    (0x28, 0x2c, 0x34) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer =
  (buildDefaultGSConfig myColorizer)
    { gs_cellheight = 40,
      gs_cellwidth = 200,
      gs_cellpadding = 6,
      gs_navigate = myNavigation,
      gs_originFractX = 0.5,
      gs_originFractY = 0.5,
      gs_font = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where
    conf =
      def
        { gs_cellheight = 40,
          gs_cellwidth = 180,
          gs_cellpadding = 6,
          gs_originFractX = 0.5,
          gs_originFractY = 0.5,
          gs_font = myFont
        }

runSelectedAction' :: GSConfig (X ()) -> [(String, X ())] -> X ()
runSelectedAction' conf actions = do
  selectedActionM <- gridselect conf actions
  case selectedActionM of
    Just selectedAction -> selectedAction
    Nothing -> return ()

gsCategories =
  [ ("Games", "xdotool key super+alt+1"),
    ("Education", "xdotool key super+alt+2"),
    ("Internet", "xdotool key super+alt+3"),
    ("Multimedia", "xdotool key super+alt+4"),
    ("Office", "xdotool key super+alt+5"),
    ("Settings", "xdotool key super+alt+6"),
    ("System", "xdotool key super+alt+7"),
    ("Utilities", "xdotool key super+alt+8")
  ]

gsGames =
  []

gsEducation =
  []

gsInternet =
  [ ("Brave Browser", "brave"),
    ("Discord", "discord"),
    ("Firefox", "firefox"),
    ("Google Chrome", "google-chrome-stable"),
    ("Zoom", "zoom")
  ]

gsMultimedia =
  [ ("OBS Studio", "obs"),
    ("VLC", "vlc")
  ]

gsOffice =
  []

gsSettings =
  []

gsSystem =
  [ ("Bash", (myTerminal ++ " -e bash")),
    ("Btop", (myTerminal ++ " -e btop")),
    ("PCManFM", "pcmanfm"),
    ("Zsh", (myTerminal ++ " -e zsh"))
  ]

gsUtilities =
  [ ("Emacs", "emacs"),
    ("Emacsclient", "emacsclient -c -a 'emacs'"),
    ("Lvim", (myTerminal ++ " -e lvim")),
    ("Nvim", (myTerminal ++ " -e nvim"))
  ]

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal" spawnTerm findTerm manageTerm,
    NS "volumecontrol" spawnVolume findVolume manageVolume
  ]
  where
    spawnTerm = myTerminal ++ " -t scratchpad"
    findTerm = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnVolume = "pavucontrol"
    findVolume = title =? "Volume Control"
    manageVolume = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w

-- Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall =
  renamed [Replace "tall"] $
    limitWindows 5 $
      smartBorders $
        windowNavigation $
          addTabs shrinkText myTabTheme $
            subLayout [] (smartBorders Simplest) $
              mySpacing 8 $
                ResizableTall 1 (3 / 100) (1 / 2) []

monocle =
  renamed [Replace "monocle"] $
    smartBorders $
      windowNavigation $
        addTabs shrinkText myTabTheme $
          subLayout [] (smartBorders Simplest) $
            Full

floats =
  renamed [Replace "floats"] $
    smartBorders $
      simplestFloat

grid =
  renamed [Replace "grid"] $
    limitWindows 9 $
      smartBorders $
        windowNavigation $
          addTabs shrinkText myTabTheme $
            subLayout [] (smartBorders Simplest) $
              mySpacing 8 $
                mkToggle (single MIRROR) $
                  Grid (16 / 10)

spirals =
  renamed [Replace "spirals"] $
    limitWindows 9 $
      smartBorders $
        windowNavigation $
          addTabs shrinkText myTabTheme $
            subLayout [] (smartBorders Simplest) $
              mySpacing' 8 $
                spiral (6 / 7)

threeCol =
  renamed [Replace "threeCol"] $
    limitWindows 7 $
      smartBorders $
        windowNavigation $
          addTabs shrinkText myTabTheme $
            subLayout [] (smartBorders Simplest) $
              ThreeCol 1 (3 / 100) (1 / 2)

threeRow =
  renamed [Replace "threeRow"] $
    limitWindows 7 $
      smartBorders $
        windowNavigation $
          addTabs shrinkText myTabTheme $
            subLayout [] (smartBorders Simplest)
            -- Mirror takes a layout and rotates it by 90 degrees.
            -- So we are applying Mirror to the ThreeCol layout.
            $
              Mirror $
                ThreeCol 1 (3 / 100) (1 / 2)

tabs =
  renamed [Replace "tabs"]
  -- I cannot add spacing to this layout because it will
  -- add spacing between window and tabs which looks bad.
  $
    tabbed shrinkText myTabTheme

tallAccordion =
  renamed [Replace "tallAccordion"] $
    Accordion

wideAccordion =
  renamed [Replace "wideAccordion"] $
    Mirror Accordion

-- setting colors for tabs layout and tabs sublayout.
myTabTheme =
  def
    { fontName = myFont,
      activeColor = color08,
      activeBorderColor = color08,
      inactiveColor = color01,
      inactiveBorderColor = color01,
      activeTextColor = color03,
      inactiveTextColor = color03
    }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme =
  def
    { swn_font = "xft:Ubuntu:bold:size=60",
      swn_fade = 0.4,
      swn_bgcolor = color01,
      swn_color = "#ffffff"
    }

-- The layout hook
myLayoutHook =
  avoidStruts $
    mouseResize $
      windowArrange $
        T.toggleLayouts floats $
          mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout =
      withBorder myBorderWidth tall
        ||| noBorders monocle
        ||| floats
        ||| noBorders tabs
        ||| grid
        ||| spirals
        ||| threeCol
        ||| threeRow
        ||| tallAccordion
        ||| wideAccordion

myWorkspaces = [" dev ", " www ", " sys ", " chat ", " disc ", " mus ", " vid ", " img ", " game "]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1 ..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
    -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
    -- I'm doing it this way because otherwise I would have to write out the full
    -- name of my workspaces and the names would be very long if using clickable workspaces.
    [ -- do float
      className =? "confirm" --> doFloat,
      className =? "file_progress" --> doFloat,
      className =? "download" --> doFloat,
      className =? "error" --> doFloat,
      className =? "Gimp" --> doFloat,
      className =? "notification" --> doFloat,
      className =? "pinentry-gtk-2" --> doFloat,
      className =? "splash" --> doFloat,
      className =? "toolbar" --> doFloat,
      (className =? "TelegramDesktop" <||> className =? "Fluffychat") --> doFloat,
      className =? "Yad" --> doCenterFloat,
      isDialog --> doCenterFloat,
      isFullscreen --> doFullFloat,
      title =? "Bluetooth Device" --> doCenterFloat,
      title =? "Volume Control" --> doCenterFloat,
      (className =? "Mozilla Firefox" <||> resource =? "Toolkit") --> doFloat,
      -- do shift
      className =? "Brave-browser" --> doShift (myWorkspaces !! 1),
      title =? "Mozilla Firefox" --> doShift (myWorkspaces !! 1),
      className =? "Google-chrome" --> doShift (myWorkspaces !! 1),
      (className =? "TelegramDesktop" <||> className =? "Fluffychat") --> doShift (myWorkspaces !! 3),
      className =? "discord" --> doShift (myWorkspaces !! 4),
      title =? "Spotify" --> doShift (myWorkspaces !! 5),
      className =? "Gimp" --> doShift (myWorkspaces !! 8),
      -- do copy to all workspaces
      --
      (title =? "Picture in picture") --> doF copyToAll, -- Brave
      (title =? "Picture-in-Picture") --> doF copyToAll, -- Firefox
      (title =? "Picture in Picture") --> doF copyToAll -- Chrome
    ]
    <+> namedScratchpadManageHook myScratchPads

subtitle' :: String -> ((KeyMask, KeySym), NamedAction)
subtitle' x =
  ( (0, 0),
    NamedAction $
      map toUpper $
        sep ++ "\n-- " ++ x ++ " --\n" ++ sep
  )
  where
    sep = replicate (6 + length x) '-'

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ "yad --text-info --fontname=\"JetBrainsMono Nerd Font 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
  -- hPutStr h (unlines $ showKm x) -- showKM adds ">>" before subtitles
  hPutStr h (unlines $ showKmSimple x) -- showKmSimple doesn't add ">>" to subtitles
  hClose h
  return ()

myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
  -- (subtitle "Custom Keys":) $ mkNamedKeymap c $
  let subKeys str ks = subtitle' str : mkNamedKeymap c ks
   in subKeys
        "Xmonad Essentials"
        [ ("M-C-r", addName "Recompile XMonad" $ spawn "xmonad --recompile"),
          ("M-S-r", addName "Restart XMonad" $ spawn "xmonad --restart"),
          ("M-S-q", addName "Quit XMonad" $ io exitSuccess),
          ("M-S-c", addName "Kill focused window" $ kill1),
          ("M-S-a", addName "Kill all windows on WS" $ killAll),
          ("M-S-b", addName "Toggle bar show/hide" $ sendMessage ToggleStruts),
          ("M-d", addName "Open Rofi launcher" $ spawn "sh $HOME/.config/rofi/bin/launcher"),
          ("M-S-e", addName "Open Rofi powermenu" $ spawn "sh $HOME/.config/rofi/bin/powermenu"),
          ("M-S-l", addName "Lock session" $ spawn "sh $HOME/.setup/dotfiles/local/bin/lock")
        ]
        ^++^ subKeys
          "Switch to workspace"
          [ ("M-1", addName "Switch to workspace 1" $ (windows $ W.greedyView $ myWorkspaces !! 0)),
            ("M-2", addName "Switch to workspace 2" $ (windows $ W.greedyView $ myWorkspaces !! 1)),
            ("M-3", addName "Switch to workspace 3" $ (windows $ W.greedyView $ myWorkspaces !! 2)),
            ("M-4", addName "Switch to workspace 4" $ (windows $ W.greedyView $ myWorkspaces !! 3)),
            ("M-5", addName "Switch to workspace 5" $ (windows $ W.greedyView $ myWorkspaces !! 4)),
            ("M-6", addName "Switch to workspace 6" $ (windows $ W.greedyView $ myWorkspaces !! 5)),
            ("M-7", addName "Switch to workspace 7" $ (windows $ W.greedyView $ myWorkspaces !! 6)),
            ("M-8", addName "Switch to workspace 8" $ (windows $ W.greedyView $ myWorkspaces !! 7)),
            ("M-9", addName "Switch to workspace 9" $ (windows $ W.greedyView $ myWorkspaces !! 8))
          ]
        ^++^ subKeys
          "Send window to workspace"
          [ ("M-S-1", addName "Send to workspace 1" $ (windows $ W.shift $ myWorkspaces !! 0)),
            ("M-S-2", addName "Send to workspace 2" $ (windows $ W.shift $ myWorkspaces !! 1)),
            ("M-S-3", addName "Send to workspace 3" $ (windows $ W.shift $ myWorkspaces !! 2)),
            ("M-S-4", addName "Send to workspace 4" $ (windows $ W.shift $ myWorkspaces !! 3)),
            ("M-S-5", addName "Send to workspace 5" $ (windows $ W.shift $ myWorkspaces !! 4)),
            ("M-S-6", addName "Send to workspace 6" $ (windows $ W.shift $ myWorkspaces !! 5)),
            ("M-S-7", addName "Send to workspace 7" $ (windows $ W.shift $ myWorkspaces !! 6)),
            ("M-S-8", addName "Send to workspace 8" $ (windows $ W.shift $ myWorkspaces !! 7)),
            ("M-S-9", addName "Send to workspace 9" $ (windows $ W.shift $ myWorkspaces !! 8))
          ]
        ^++^ subKeys
          "Move window to WS and go there"
          [ ("M-S-<Page_Up>", addName "Move window to next WS" $ shiftTo Next nonNSP >> moveTo Next nonNSP),
            ("M-S-<Page_Down>", addName "Move window to prev WS" $ shiftTo Prev nonNSP >> moveTo Prev nonNSP)
          ]
        ^++^ subKeys
          "Window navigation"
          [ ("M-j", addName "Move focus to next window" $ windows W.focusDown),
            ("M-k", addName "Move focus to prev window" $ windows W.focusUp),
            ("M-m", addName "Move focus to master window" $ windows W.focusMaster),
            ("M-S-j", addName "Swap focused window with next window" $ windows W.swapDown),
            ("M-S-k", addName "Swap focused window with prev window" $ windows W.swapUp),
            ("M-S-m", addName "Swap focused window with master window" $ windows W.swapMaster),
            ("M-<Backspace>", addName "Move focused window to master" $ promote),
            ("M-S-,", addName "Rotate all windows except master" $ rotSlavesDown),
            ("M-S-.", addName "Rotate all windows current stack" $ rotAllDown)
          ]
        ^++^ subKeys
          "Favorite programs"
          [ ("M-<Return>", addName "Launch terminal" $ spawn (myTerminal)),
            ("M-b", addName "Launch web browser" $ spawn (myBrowser)),
            ("M-M1-h", addName "Launch btop" $ spawn (myTerminal ++ " -e btop"))
          ]
        ^++^ subKeys
          "Monitors"
          [ ("M-.", addName "Switch focus to next monitor" $ nextScreen),
            ("M-,", addName "Switch focus to prev monitor" $ prevScreen)
          ]
        -- Switch layouts
        ^++^ subKeys
          "Switch layouts"
          [ ("M-<Tab>", addName "Switch to next layout" $ sendMessage NextLayout),
            ("M-<Space>", addName "Toggle noborders/full" $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
          ]
        -- Window resizing
        ^++^ subKeys
          "Window resizing"
          [ ("M-h", addName "Shrink window" $ sendMessage Shrink),
            ("M-l", addName "Expand window" $ sendMessage Expand),
            ("M-M1-j", addName "Shrink window vertically" $ sendMessage MirrorShrink),
            ("M-M1-k", addName "Expand window vertically" $ sendMessage MirrorExpand)
          ]
        -- Floating windows
        ^++^ subKeys
          "Floating windows"
          [ ("M-f", addName "Toggle float layout" $ sendMessage (T.Toggle "floats")),
            ("M-t", addName "Sink a floating window" $ withFocused $ windows . W.sink),
            ("M-S-t", addName "Sink all floated windows" $ sinkAll)
          ]
        -- Increase/decrease spacing (gaps)
        ^++^ subKeys
          "Window spacing (gaps)"
          [ ("C-M1-j", addName "Decrease window spacing" $ decWindowSpacing 4),
            ("C-M1-k", addName "Increase window spacing" $ incWindowSpacing 4),
            ("C-M1-h", addName "Decrease screen spacing" $ decScreenSpacing 4),
            ("C-M1-l", addName "Increase screen spacing" $ incScreenSpacing 4)
          ]
        -- Increase/decrease windows in the master pane or the stack
        ^++^ subKeys
          "Increase/decrease windows in master pane or the stack"
          [ ("M-S-<Up>", addName "Increase clients in master pane" $ sendMessage (IncMasterN 1)),
            ("M-S-<Down>", addName "Decrease clients in master pane" $ sendMessage (IncMasterN (-1))),
            ("M-=", addName "Increase max # of windows for layout" $ increaseLimit),
            ("M--", addName "Decrease max # of windows for layout" $ decreaseLimit)
        -- Sublayouts
          ]
        -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        ^++^ subKeys
          "Sublayouts"
          [ ("M-C-h", addName "pullGroup L" $ sendMessage $ pullGroup L),
            ("M-C-l", addName "pullGroup R" $ sendMessage $ pullGroup R),
            ("M-C-k", addName "pullGroup U" $ sendMessage $ pullGroup U),
            ("M-C-j", addName "pullGroup D" $ sendMessage $ pullGroup D),
            ("M-C-m", addName "MergeAll" $ withFocused (sendMessage . MergeAll)),
            -- , ("M-C-u", addName "UnMerge"               $ withFocused (sendMessage . UnMerge))
            ("M-C-/", addName "UnMergeAll" $ withFocused (sendMessage . UnMergeAll)),
            ("M-C-.", addName "Switch focus next tab" $ onGroup W.focusUp'),
            ("M-C-,", addName "Switch focus prev tab" $ onGroup W.focusDown')
          ]
        -- Scratchpads
        -- Toggle show/hide these programs. They run on a hidden workspace.
        -- When you toggle them to show, it brings them to current workspace.
        -- Toggle them to hide and it sends them back to hidden workspace (NSP).
        ^++^ subKeys
          "Scratchpads"
          [ ("M-s t", addName "Toggle scratchpad terminal" $ namedScratchpadAction myScratchPads "terminal"),
            ("M-s p", addName "Toggle scratchpad pavucontrol" $ namedScratchpadAction myScratchPads "volumecontrol")
          ]
        ^++^ subKeys
          "GridSelect"
          -- , ("C-g g", addName "Select favorite apps"     $ runSelectedAction' defaultGSConfig gsCategories)
          [ ( "M-M1-<Return>",
              addName "Select favorite apps" $
                spawnSelected' $
                  gsGames ++ gsEducation ++ gsInternet ++ gsMultimedia ++ gsOffice ++ gsSettings ++ gsSystem ++ gsUtilities
            ),
            ("M-M1-c", addName "Select favorite apps" $ spawnSelected' gsCategories),
            ("M-M1-t", addName "Goto selected window" $ goToSelected $ mygridConfig myColorizer),
            ("M-M1-b", addName "Bring selected window" $ bringSelected $ mygridConfig myColorizer),
            ("M-M1-1", addName "Menu of games" $ spawnSelected' gsGames),
            ("M-M1-2", addName "Menu of education apps" $ spawnSelected' gsEducation),
            ("M-M1-3", addName "Menu of Internet apps" $ spawnSelected' gsInternet),
            ("M-M1-4", addName "Menu of multimedia apps" $ spawnSelected' gsMultimedia),
            ("M-M1-5", addName "Menu of office apps" $ spawnSelected' gsOffice),
            ("M-M1-6", addName "Menu of settings apps" $ spawnSelected' gsSettings),
            ("M-M1-7", addName "Menu of system apps" $ spawnSelected' gsSystem),
            ("M-M1-8", addName "Menu of utilities apps" $ spawnSelected' gsUtilities)
          ]
        -- Multimedia Keys
        ^++^ subKeys
          "Multimedia keys"
          [ ("<XF86AudioPlay>", addName "playerctl play-pause" $ spawn "playerctl play-pause"),
            ("<XF86AudioPrev>", addName "playerctl previous" $ spawn "playerctl previous"),
            ("<XF86AudioNext>", addName "playerctl next" $ spawn "playerctl next"),
            ("<XF86AudioMute>", addName "Toggle audio mute" $ spawn "amixer set Master toggle"),
            ("<XF86AudioLowerVolume>", addName "Lower volume" $ spawn "amixer set Master 5%- unmute"),
            ("<XF86AudioRaiseVolume>", addName "Raise volume" $ spawn "amixer set Master 5%+ unmute"),
            ("<Print>", addName "Open Rofi screenshot" $ spawn "sh $HOME/.config/rofi/bin/screenshot"),
            ("M-m p", addName "Open Rofi music" $ spawn "sh $HOME/.config/rofi/bin/music")
          ]
  where
    -- The following lines are needed for named scratchpads.
    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))
    nonEmptyNonNSP = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar -x 0 $HOME/.setup/modules/desktops/xmonad/xmobar/xmobarrc"
  xmonad $
    addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) myKeys $
      docks . ewmhFullscreen . ewmh $
        def
          { manageHook = myManageHook <+> manageDocks,
            handleEventHook = windowedFullscreenFixEventHook <> swallowEventHook (className =? "Alacritty" <||> className =? "org.wezfurlong.wezterm" <||> className =? "st-256color" <||> className =? "XTerm") (return True) <> trayerPaddingXmobarEventHook,
            modMask = myModMask,
            terminal = myTerminal,
            startupHook = myStartupHook,
            layoutHook = showWName' myShowWNameTheme $ myLayoutHook,
            workspaces = myWorkspaces,
            borderWidth = myBorderWidth,
            normalBorderColor = myNormColor,
            focusedBorderColor = myFocusColor,
            logHook =
              dynamicLogWithPP $
                filterOutWsPP [scratchpadWorkspaceTag] $
                  xmobarPP
                    { ppOutput = hPutStrLn xmproc,
                      ppCurrent = xmobarColor color08 "" . wrap "[ " " ]",
                      ppVisible = xmobarColor color03 "" . clickable,
                      ppHidden =
                        xmobarColor color01 ""
                          . wrap
                            ("<fc=" ++ color03 ++ ">")
                            "</fc>"
                          . clickable,
                      ppHiddenNoWindows = xmobarColor color02 "" . clickable,
                      ppTitle = xmobarColor color09 "" . shorten 60,
                      ppSep = "<fc=" ++ color02 ++ ">    <fn=2>\xf054</fn>    </fc>",
                      ppUrgent = xmobarColor color10 "" . wrap "!" "!",
                      ppExtras = [windowCount],
                      ppOrder = \(ws : l : t : ex) -> ["<fn=4>" ++ ws ++ "</fn>"] ++ ex ++ ["<fc=" ++ color08 ++ ">[" ++ l ++ "]</fc>  " ++ t]
                    }
          }
