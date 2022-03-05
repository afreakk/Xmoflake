module BackAndForth (backAndForth) where

import XMonad
import qualified XMonad.Hooks.WorkspaceHistory as WH
import XMonad.StackSet

-- needs
--  workspaceHistoryHook
--  attatched to ur loghook

backAndForth :: WorkspaceId -> X ()
backAndForth toWS = gets (currentTag . windowset) >>= go
    where
        go cur
          | toWS == cur = WH.workspaceHistory >>= showSndWindowInList
          | otherwise   = windows (greedyView toWS)

showSndWindowInList :: [WorkspaceId] -> X ()
showSndWindowInList (_:znd:_) = windows (greedyView znd)
showSndWindowInList _         = return ()

