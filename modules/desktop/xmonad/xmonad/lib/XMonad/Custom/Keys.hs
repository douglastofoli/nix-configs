module XMonad.Custom.Keys (myKeys, showKeybindings) where

import Data.Char (toUpper)
import Data.Maybe
import System.Exit (exitSuccess)
import System.IO (hClose, hPutStr)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D (..), WSType (..), moveTo, nextScreen, nextWS, prevScreen, prevWS, shiftTo)
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.Minimize
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import XMonad.Actions.Search qualified as S
import XMonad.Actions.Submap
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Custom.GridSelect qualified as C
import XMonad.Custom.Scratchpads qualified as C
import XMonad.Custom.Variables qualified as C
import XMonad.Custom.Workspaces qualified as C
import XMonad.Custom.XPrompt qualified as C
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LimitWindows
import XMonad.Layout.MultiToggle qualified as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.ToggleLayouts qualified as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Operations
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.OrgMode
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (mkNamedKeymap)
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

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
  h <- spawnPipe $ "yad --text-info --fontname=\"SauceCodePro Nerd Font Mono 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
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
          ("M-S-<Return>", addName "Run rofi prompt" $ spawn "rofi -dmenu -p 'Run command:'"),
          ("M-d", addName "Run rofi apps" $ spawn "rofi -no-lazy-grab -show drun"),
          ("M-S-b", addName "Toggle bar show/hide" $ sendMessage ToggleStruts)
        ]
        ^++^ subKeys
          "Switch to workspace"
          [ ("M-1", addName "Switch to workspace 1" $ (windows $ W.greedyView $ C.myWorkspaces !! 0)),
            ("M-2", addName "Switch to workspace 2" $ (windows $ W.greedyView $ C.myWorkspaces !! 1)),
            ("M-3", addName "Switch to workspace 3" $ (windows $ W.greedyView $ C.myWorkspaces !! 2)),
            ("M-4", addName "Switch to workspace 4" $ (windows $ W.greedyView $ C.myWorkspaces !! 3)),
            ("M-5", addName "Switch to workspace 5" $ (windows $ W.greedyView $ C.myWorkspaces !! 4)),
            ("M-6", addName "Switch to workspace 6" $ (windows $ W.greedyView $ C.myWorkspaces !! 5)),
            ("M-7", addName "Switch to workspace 7" $ (windows $ W.greedyView $ C.myWorkspaces !! 6)),
            ("M-8", addName "Switch to workspace 8" $ (windows $ W.greedyView $ C.myWorkspaces !! 7)),
            ("M-9", addName "Switch to workspace 9" $ (windows $ W.greedyView $ C.myWorkspaces !! 8))
          ]
        ^++^ subKeys
          "Send window to workspace"
          [ ("M-S-1", addName "Send to workspace 1" $ (windows $ W.shift $ C.myWorkspaces !! 0)),
            ("M-S-2", addName "Send to workspace 2" $ (windows $ W.shift $ C.myWorkspaces !! 1)),
            ("M-S-3", addName "Send to workspace 3" $ (windows $ W.shift $ C.myWorkspaces !! 2)),
            ("M-S-4", addName "Send to workspace 4" $ (windows $ W.shift $ C.myWorkspaces !! 3)),
            ("M-S-5", addName "Send to workspace 5" $ (windows $ W.shift $ C.myWorkspaces !! 4)),
            ("M-S-6", addName "Send to workspace 6" $ (windows $ W.shift $ C.myWorkspaces !! 5)),
            ("M-S-7", addName "Send to workspace 7" $ (windows $ W.shift $ C.myWorkspaces !! 6)),
            ("M-S-8", addName "Send to workspace 8" $ (windows $ W.shift $ C.myWorkspaces !! 7)),
            ("M-S-9", addName "Send to workspace 9" $ (windows $ W.shift $ C.myWorkspaces !! 8))
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
        -- \^++^ subKeys
        --   "Prompts"
        --   [ ("M-r <Space>", shellPrompt C.myXPConfig),
        --     ("M-r o", orgPrompt C.myXPConfig "TODO" "$HOME/org-mode/todo.org")
        --   ]
        ^++^ subKeys
          "Favorite programs"
          [ ("M-<Return>", addName "Launch terminal" $ spawn (C.myTerminal)),
            ("M-b", addName "Launch web browser" $ spawn (C.myBrowser)),
            ("M-M1-h", addName "Launch btop" $ spawn (C.myTerminal ++ " -e btop"))
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
          ]
        -- Sublayouts
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
          [ ("M-s t", addName "Toggle scratchpad terminal" $ namedScratchpadAction C.myScratchPads "terminal"),
            ("M-s m", addName "Toggle scratchpad mocp" $ namedScratchpadAction C.myScratchPads "mocp"),
            ("M-s c", addName "Toggle scratchpad calculator" $ namedScratchpadAction C.myScratchPads "calculator")
          ]
        -- Controls for mocp music player (SUPER-u followed by a key)
        ^++^ subKeys
          "Mocp music player"
          [ ("M-u p", addName "mocp play" $ spawn "mocp --play"),
            ("M-u l", addName "mocp next" $ spawn "mocp --next"),
            ("M-u h", addName "mocp prev" $ spawn "mocp --previous"),
            ("M-u <Space>", addName "mocp toggle pause" $ spawn "mocp --toggle-pause")
          ]
        -- \^++^ subKeys
        --   "GridSelect"
        --   [ ( "M-M1-<Return>",
        --       addName "Select favorite apps" $
        --         spawnSelected' $
        --           gsGames ++ gsEducation ++ gsInternet ++ gsMultimedia ++ gsOffice ++ gsSettings ++ gsSystem ++ gsUtilities
        --     ),
        --     ("M-M1-c", addName "Select favorite apps" $ spawnSelected' gsCategories),
        --     ("M-M1-t", addName "Goto selected window" $ goToSelected $ mygridConfig myColorizer),
        --     ("M-M1-b", addName "Bring selected window" $ bringSelected $ mygridConfig myColorizer),
        --     ("M-M1-1", addName "Menu of games" $ spawnSelected' gsGames),
        --     ("M-M1-2", addName "Menu of education apps" $ spawnSelected' gsEducation),
        --     ("M-M1-3", addName "Menu of Internet apps" $ spawnSelected' gsInternet),
        --     ("M-M1-4", addName "Menu of multimedia apps" $ spawnSelected' gsMultimedia),
        --     ("M-M1-5", addName "Menu of office apps" $ spawnSelected' gsOffice),
        --     ("M-M1-6", addName "Menu of settings apps" $ spawnSelected' gsSettings),
        --     ("M-M1-7", addName "Menu of system apps" $ spawnSelected' gsSystem),
        --     ("M-M1-8", addName "Menu of utilities apps" $ spawnSelected' gsUtilities)
        --   ]
        -- Emacs (SUPER-e followed by a key)
        ^++^ subKeys
          "Emacs"
          [ ("M-e e", addName "Emacsclient" $ spawn (C.myEmacs)),
            ("M-e a", addName "Emacsclient EMMS (music)" $ spawn (C.myEmacs ++ ("--eval '(emms)' --eval '(emms-play-directory-tree \"~/Music/\")'"))),
            ("M-e b", addName "Emacsclient Ibuffer" $ spawn (C.myEmacs ++ ("--eval '(ibuffer)'"))),
            ("M-e d", addName "Emacsclient Dired" $ spawn (C.myEmacs ++ ("--eval '(dired nil)'"))),
            ("M-e i", addName "Emacsclient ERC (IRC)" $ spawn (C.myEmacs ++ ("--eval '(erc)'"))),
            ("M-e n", addName "Emacsclient Elfeed (RSS)" $ spawn (C.myEmacs ++ ("--eval '(elfeed)'"))),
            ("M-e s", addName "Emacsclient Eshell" $ spawn (C.myEmacs ++ ("--eval '(eshell)'"))),
            ("M-e v", addName "Emacsclient Vterm" $ spawn (C.myEmacs ++ ("--eval '(+vterm/here nil)'"))),
            ("M-e w", addName "Emacsclient EWW Browser" $ spawn (C.myEmacs ++ ("--eval '(doom/window-maximize-buffer(eww \"distro.tube\"))'")))
          ]
        -- Multimedia Keys
        ^++^ subKeys
          "Multimedia keys"
          [ ("<XF86AudioPlay>", addName "mocp play" $ spawn "mocp --play"),
            ("<XF86AudioPrev>", addName "mocp next" $ spawn "mocp --previous"),
            ("<XF86AudioNext>", addName "mocp prev" $ spawn "mocp --next"),
            ("<XF86AudioMute>", addName "Toggle audio mute" $ spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
            ("<XF86AudioLowerVolume>", addName "Lower vol" $ spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"),
            ("<XF86AudioRaiseVolume>", addName "Raise vol" $ spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"),
            ("<XF86Calculator>", addName "Calculator" $ runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk")),
            ("M-<Print>", addName "Take screenshot" $ spawn "flameshot gui -p $HOME/Pictures/Screenshots")
          ]
  where
    -- The following lines are needed for named scratchpads.
    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))
    nonEmptyNonNSP = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
