module GridSelects ( gsWithWindows, gsWindowGoto, gsActionRunner ) where

import XMonad
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

gsWindowGoto cfg = goToSelected $ (myGsConfigWithDefaultColorizer cfg) {gs_cellwidth = cl_gsCellWidthBig cfg }

gsActionRunner actions cfg = do
    runSelectedAction (myGsConfig cfg) actions


-- configs

myGsConfigWithDefaultColorizer cfg = def
            { gs_font = cl_font cfg
            , gs_cellwidth = cl_gsCellWidth cfg
            , gs_navigate = navNSearch
            , gs_cellheight = cl_gsCellHeight cfg
            }

myGsConfig cfg = (buildDefaultGSConfig colorizer)
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
