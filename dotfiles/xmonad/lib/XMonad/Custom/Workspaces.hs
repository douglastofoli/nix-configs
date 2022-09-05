module XMonad.Custom.Workspaces where

import qualified Data.Map as M
import Data.Maybe

myWorkspaces :: [String]
myWorkspaces = [" www ", " dev ", " sys ", " chat ", " disc ", " mus ", " vbox ", " vid ", " gfx "]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices
