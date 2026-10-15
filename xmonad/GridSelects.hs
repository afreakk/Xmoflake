module GridSelects ( gsWithWindows, gsWindowGoto, gsClaudeGoto, gsActionRunner ) where

import Data.List (isPrefixOf)
import XMonad
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W
import XMonad.Actions.GridSelect
import AConfig (AConfig (..))

-- actions

gsWithWindows extraActions cfg = withFocused $ \w -> do
    tags <- asks (workspaces . config)
    runSelectedAction
        ( myGsConfigBigFonts cfg )
        ( extraActions
            ++ [ ("Move to " ++ tag, windows $ W.shift tag)
            | tag <- tags ] )

gsWindowGoto cfg = goToSelected $ (myGsConfig cfg) {gs_cellwidth = cl_gsCellWidthBig cfg }

-- Like gsWindowGoto, but only Claude Code sessions. Claude titles its
-- terminal "<status> <topic>" (✳ idle, ◐/◑ working), which tmux passes on
-- to the alacritty window via set-titles.
gsClaudeGoto cfg = do
    wins <- gets (W.allWindows . windowset)
    named <- mapM (\w -> (\n -> (show n, w)) <$> getName w) wins
    let claude = filter (\(t, _) -> any (`isPrefixOf` t) (claudeIdle : claudeWorking)) named
    selected <- gridselect ((gsConfigWith (claudeColorizer cfg) cfg) {gs_cellwidth = cl_gsCellWidthBig cfg}) claude
    whenJust selected (windows . W.focusWindow)

claudeIdle :: String
claudeIdle = "\x2733 "
claudeWorking :: [String]
claudeWorking = ["\x25D0 ", "\x25D1 "]

-- Idle sessions (waiting for you) stand out in the "fine" colour; working
-- ones stay plain.
claudeColorizer :: AConfig -> Window -> Bool -> X (String, String)
claudeColorizer cfg w active = do
    t <- show <$> getName w
    return $ if active then (cl_accent cfg, cl_bg cfg)
             else if claudeIdle `isPrefixOf` t then (cl_finecolor cfg, cl_bg cfg)
             else (cl_bg cfg, cl_fg cfg)

gsActionRunner actions cfg = do
    runSelectedAction (myGsConfig cfg) actions


-- configs

myGsConfig = gsConfigWith colorizer

gsConfigWith colorize cfg = (buildDefaultGSConfig colorize)
            { gs_font = cl_font cfg
            , gs_cellwidth = cl_gsCellWidth cfg
            , gs_navigate = navNSearch
            , gs_cellheight = cl_gsCellHeight cfg
            }

myGsConfigBigFonts cfg = (myGsConfig cfg)
            { gs_font = cl_font_big cfg
            , gs_cellwidth = cl_gsCellWidthBig cfg
            , gs_cellheight = cl_gsCellHeightBig cfg
            }

colorizer :: a -> Bool -> X (String, String)
colorizer _ isFg = do
    fBC <- asks (focusedBorderColor . config)
    nBC <- asks (normalBorderColor . config)
    return $ if isFg
                then (fBC, nBC)
                else (nBC, fBC)

-- retired
-- gsWindowGoto cfg = do
--     mSelWin <- gridselectWindow $ (myGsConfig cfg) {gs_cellwidth = 240}
--     case mSelWin of
--       Just selWin -> windows $ W.focusWindow selWin
--       Nothing -> return ()
