{-# LANGUAGE FlexibleContexts #-}

module LayoutHook (myLayout) where

import AConfig (AConfig (..))
import XMonad.Hooks.ManageDocks as MD
import XMonad.Layout
-- import BooleanLayout
import XMonad.Layout.BoringWindows as BRNG
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation

myTabConfig :: AConfig -> XMonad.Layout.Tabbed.Theme
myTabConfig cfg =
  def
    { activeTextColor = cl_accent cfg,
      activeColor = cl_bg cfg,
      activeBorderColor = cl_accent cfg,
      inactiveTextColor = cl_fg cfg,
      inactiveColor = cl_bg cfg,
      inactiveBorderColor = cl_fg cfg,
      urgentTextColor = cl_alert cfg,
      urgentColor = cl_bg cfg,
      urgentBorderColor = cl_alert cfg,
      fontName = cl_font cfg,
      decoHeight = fromIntegral $ cl_tabHeight cfg
    }

------------------------------------------------------------------------
-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout cfg = gaps $ smartBorders $ MD.avoidStruts $ BRNG.boringWindows $ {-wrappedHogwartsTiled ||| -} wrappedTiled ||| tabbed
  where
    gaps = spacingRaw False (Border 0 0 0 0) False (Border 10 0 5 5) True
    tabcfg = myTabConfig cfg
    tabbed = tabbedBottom shrinkText tabcfg

    -- wrappedHogwartsTiled = configurableNavigation noNavigateBorders $ subTabbedBottom tabcfg hogwartsTiled
    wrappedTiled = configurableNavigation noNavigateBorders $ subTabbedBottom tabcfg tiled

    -- dynamicTiledSubTabbed = configurableNavigation noNavigateBorders $ subTabbedBottom tabcfg dynamicTiled
    -- dynamicTiled          = BooleanLayout isHogwarts hogwartsTiled tiled
    -- hogwartsTiled = reflectHoriz $ reflectVert $ Mirror tiled
    tiled = ResizableTall nmaster delta ratio []

    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane (magic number at the end is so terminal lines align better with monitor at home)
    ratio = toRational ((2 / (1 + sqrt 5 :: Double)) + fromIntegral (cl_barHeight cfg) / 2160) + 0.0929
    -- Percent of screen to increment by when resizing panes
    delta = 25 / 1000
    subTabbedBottom tabcfg parentLayout = addTabsBottom shrinkText tabcfg $ subLayout [] Simplest parentLayout

-- isHogwarts = cl_hostName cfg == "hogwarts"
