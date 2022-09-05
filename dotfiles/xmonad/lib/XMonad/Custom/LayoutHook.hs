module XMonad.Custom.LayoutHook (myLayoutHook) where

  -- Base
import XMonad

  -- Actions
import XMonad.Actions.MouseResize

  -- Hooks
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))

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

  -- Custom
import qualified XMonad.Custom.Workspaces as C
import qualified XMonad.Custom.Colors.Dracula as C
import qualified XMonad.Custom.Variables as C

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
    fontName = C.myFont
  , activeColor = C.color15
  , inactiveColor = C.color08
  , activeBorderColor = C.color15
  , inactiveBorderColor = C.colorBack
  , activeTextColor = C.colorBack
  , inactiveTextColor = C.color16
  }

myLayoutHook = avoidStruts 
               $ mouseResize 
               $ windowArrange 
               $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = withBorder C.myBorderWidth tall
                                              ||| noBorders monocle
                                              ||| floats
                                              ||| noBorders tabs
                                              ||| grid
                                              ||| spirals
                                              ||| threeCol
                                              ||| threeRow
                                              ||| tallAccordion
                                              ||| wideAccordion