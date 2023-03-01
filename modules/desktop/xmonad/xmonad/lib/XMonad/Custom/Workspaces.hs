module XMonad.Custom.Workspaces where

import Data.Map qualified as M
import Data.Maybe

myWorkspaces :: [String]
myWorkspaces = [" dev ", " www ", " term ", " telg ", " disc ", " mus ", " vid ", " img ", " game "]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1 ..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices
