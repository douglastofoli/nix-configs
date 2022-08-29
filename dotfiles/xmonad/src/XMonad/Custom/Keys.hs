module XMonad.Custom.Keys (myKeys) where

  -- Base
import XMonad

  -- Utilities
import XMonad.Util.SpawnOnce

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

  ^++^ subKeys "Window spacing (gaps)"
  [ ("C-M1-j", addName "Decrease window spacing" $ decWindowSpacing 4)
  , ("C-M1-k", addName "Increase window spacing" $ incWindowSpacing 4)
  , ("C-M1-h", addName "Decrease screen spacing" $ decScreenSpacing 4)
  , ("C-M1-l", addName "Increase screen spacing" $ incScreenSpacing 4)]

  ^++^ subKeys "Increase/decrease windows in master pane or the stack"
  [ ("M-S-<Up>", addName "Increase clients in master pane"   $ sendMessage (IncMasterN 1))
  , ("M-S-<Down>", addName "Decrease clients in master pane" $ sendMessage (IncMasterN (-1)))
  , ("M-=", addName "Increase max # of windows for layout"   $ increaseLimit)
  , ("M--", addName "Decrease max # of windows for layout"   $ decreaseLimit)]


  ^++^ subKeys "Sublayouts"
  [ ("M-C-h", addName "pullGroup L"           $ sendMessage $ pullGroup L)
  , ("M-C-l", addName "pullGroup R"           $ sendMessage $ pullGroup R)
  , ("M-C-k", addName "pullGroup U"           $ sendMessage $ pullGroup U)
  , ("M-C-j", addName "pullGroup D"           $ sendMessage $ pullGroup D)
  , ("M-C-m", addName "MergeAll"              $ withFocused (sendMessage . MergeAll))
  -- , ("M-C-u", addName "UnMerge"               $ withFocused (sendMessage . UnMerge))
  , ("M-C-/", addName "UnMergeAll"            $  withFocused (sendMessage . UnMergeAll))
  , ("M-C-.", addName "Switch focus next tab" $  onGroup W.focusUp')
  , ("M-C-,", addName "Switch focus prev tab" $  onGroup W.focusDown')]


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
