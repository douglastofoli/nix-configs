import Data.Maybe (fromJust)
import Data.Monoid
import qualified Data.Map as M
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.CopyWindow (copyToAll, kill1)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen, isInProperty)
import XMonad.Layout.NoBorders (smartBorders, withBorder)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Hacks (trayerPaddingXmobarEventHook)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Layout.WindowArranger (windowArrange)
import Graphics.X11.Xlib.Extras

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "$TERMINAL"

myBrowser :: String
myBrowser = "$BROWSER"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor = color01

myFocusColor :: String
myFocusColor = color08

-- (Dracula Theme)
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

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

checkAndUpdateCorners :: X ()
checkAndUpdateCorners = do
    winset <- gets windowset
    let ws = W.workspace (W.current winset)
        stack = W.stack ws
        numWindows = length $ W.integrate' stack
        value = if numWindows == 1 then 0 else 1
    
    case stack of
        Just st -> do
            let wins = W.integrate st
            withDisplay $ \dpy -> do
                atom <- getAtom "_PICOM_ROUNDED"
                mapM_ (\win -> io $ changeProperty32 dpy win atom atom propModeReplace [value]) wins
        Nothing -> return ()

-- Startup Hook
myStartupHook :: X ()
myStartupHook = do
  spawn "killall trayer"
  spawnOnce "feh --bg-fill $HOME/.config/wallpaper.jpg"
  spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 " ++ colorTrayer ++ " --height 30")
  spawnOnce "sleep 2 && zen"
  spawnOnce "sleep 2 && telegram-desktop"

-- Layouts
myLayoutHook = avoidStruts $
    windowArrange $
    mkToggle (NBFULL ?? NOBORDERS ?? EOT) $
    spacingRaw True (Border 6 6 6 6) True (Border 6 6 6 6) True $
    smartBorders $
    withBorder myBorderWidth $
    tiled ||| Full ||| Mirror tiled ||| ThreeCol 1 (3/100) (1/2)
  where
    tiled = Tall 1 (3/100) (1/2)

-- Workspaces
myWorkspaces = [" dev ", " www ", " sys ", " chat ", " disc ", " mus ", " vid ", " img ", " irc "]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

-- Window Management
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "confirm" --> doFloat
    , className =? "dialog" --> doFloat
    , className =? "download" --> doFloat
    , className =? "error" --> doFloat
    , className =? "notification" --> doFloat
    , className =? "pinentry-gtk-2" --> doFloat
    , className =? "TelegramDesktop" --> doFloat
    , className =? "Yad" --> doCenterFloat
    , isDialog --> doCenterFloat
    , isFullscreen --> doFullFloat
    , className =? "zen" --> doShift (myWorkspaces !! 1)
    , className =? "TelegramDesktop" --> doShift (myWorkspaces !! 3)
    , className =? "discord" --> doShift (myWorkspaces !! 4)
    , title =? "Spotify" --> doShift (myWorkspaces !! 5)
    , title =? "Picture in Picture" --> doF copyToAll
    , title =? "Picture-in-Picture" --> doF copyToAll
    , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG" --> doCenterFloat
    , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_UTILITY" --> doCenterFloat
    , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_TOOLBAR" --> doFloat
    , isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH" --> doFloat
    ]

-- Keybindings
myKeys :: [(String, X ())]
myKeys =
    [ ("M-C-r", spawn "xmonad --recompile")
    , ("M-S-r", spawn "xmonad --restart")
    , ("M-S-q", io exitSuccess)
    , ("M-S-c", kill1)
    , ("M-S-a", killAll)
    , ("M-S-b", sendMessage ToggleStruts)
    , ("M-d", spawn "sh $HOME/.config/rofi/bin/launcher")
    , ("M-S-e", spawn "sh $HOME/.config/rofi/bin/powermenu")
    , ("M-S-l", spawn "sh $HOME/.setup/dotfiles/local/bin/lock")
    
    -- Applications
    , ("M-<Return>", spawn myTerminal)
    , ("M-b", spawn myBrowser)
    , ("M-M1-h", spawn (myTerminal ++ " -e btop"))
    
    -- Layout control
    , ("M-<Tab>", sendMessage NextLayout)
    , ("M-<Space>", sendMessage $ MT.Toggle NBFULL)
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    
    -- Windows control
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-m", windows W.focusMaster)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-S-m", windows W.swapMaster)
    
    -- Multimedia
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    , ("<Print>", spawn "sh $HOME/.config/rofi/bin/screenshot")

    -- Toggle borders
    , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)
    ] ++

    -- Workspace switching
    [("M-" ++ show n, windows $ W.greedyView $ myWorkspaces !! (n-1)) | n <- [1..9]] ++
    [("M-S-" ++ show n, windows $ W.shift $ myWorkspaces !! (n-1)) | n <- [1..9]]

-- Main
main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar -x 0 $HOME/.setup/modules/desktops/xmonad/xmobar/xmobarrc"
  xmonad $
    docks . ewmhFullscreen . ewmh $
      def
        { manageHook = myManageHook <+> manageDocks
        , handleEventHook = trayerPaddingXmobarEventHook
        , modMask = myModMask
        , terminal = myTerminal
        , startupHook = myStartupHook
        , layoutHook = myLayoutHook
        , workspaces = myWorkspaces
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = do
            dynamicLogWithPP $
              xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppCurrent = xmobarColor color08 "" . wrap "[ " " ]"
                , ppVisible = xmobarColor color03 "" . clickable
                , ppHidden =
                    xmobarColor color01 ""
                      . wrap
                        ("<fc=" ++ color03 ++ ">")
                        "</fc>"
                      . clickable
                , ppHiddenNoWindows = xmobarColor color02 "" . clickable
                , ppTitle = xmobarColor color09 "" . shorten 60
                , ppSep = "<fc=" ++ color02 ++ ">    <fn=2>\xf054</fn>    </fc>"
                , ppUrgent = xmobarColor color10 "" . wrap "!" "!"
                , ppLayout = \x -> case x of
                    "Spacing Tall" -> "Tall"
                    "Spacing Full" -> "Full"
                    "Spacing Mirror Tall" -> "Mirror"
                    "Spacing ThreeCol" -> "ThreeCol"
                    _ -> x
                , ppOrder = \(ws : l : t : ex) -> ["<fn=4>" ++ ws ++ "</fn>"] ++ ["<fc=" ++ color08 ++ ">[" ++ l ++ "]</fc>"] ++ [t]
                }
            checkAndUpdateCorners  
        } `additionalKeysP` myKeys
