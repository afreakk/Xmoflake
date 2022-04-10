{-# OPTIONS_GHC -Wno-missing-signatures #-}
import AConfig (getConfig, AConfig (..), HstNm (HstNm), hstNmCond)
import System.Exit
import qualified System.Environment as SE
import qualified System.FilePath as SF
import XMonad as XM
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.Promote
import XMonad.Actions.Submap
import XMonad.Actions.WorkspaceNames
import qualified XMonad.Hooks.FloatNext as FN
import XMonad.Hooks.ManageDocks as MD
import XMonad.Hooks.RefocusLast
-- import XMonad.Hooks.ScreenCorners
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import XMonad.Layout.BoringWindows as BRNG
import XMonad.Layout.ResizableTile
import XMonad.Layout.SubLayouts
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Actions.FloatKeys
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Prompt.FuzzyMatch
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Util.Hacks as Hacks
import qualified XMonad.Hooks.StatusBar as SB
import qualified XMonad.Hooks.StatusBar.PP as SBPP

import BackAndForth (backAndForth)
import Calculator (calculatorPrompt)
import qualified XMonad.Hooks.EwmhDesktops as EWMH
import ExtraKeyCodes
import GridSelects ({- gsWithWindows, -} gsWindowGoto, gsActionRunner)
import LayoutHook (myLayout)
import NamedScratchpadRefocusLast
import PassFork
import Utils (floatingTermClass, alacrittyFloatingOpt)
import XMonad.Layout.LayoutModifier
import qualified XMonad.Util.Run as XUR
import qualified XMonad.Actions.EasyMotion as EM
-- import XMonad.Hooks.TaffybarPagerHints (pagerHints)

newtype ModeName
 = ModeName {getModeName :: Maybe String}
 deriving (Typeable, Read, Show)

instance ExtensionClass ModeName where
  initialValue = ModeName Nothing
  extensionType = PersistentExtension

-- modalmap s = submap $ M.map (>> modalmap s) s
modalmap :: M.Map (KeyMask, KeySym) (X ()) -> String -> X ()
modalmap s modeName = do
  XS.put (ModeName (Just modeName))
  ask >>= logHook . config
  submap $ M.map (>> modalmap s modeName) s
  XS.put (ModeName Nothing)
  ask >>= logHook . config

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "spotify" "spotifywm" (className =? "Spotify")       (customFloating $ W.RationalRect 0.5 0.01 0.5 0.98)
    , NS "todo"    namedVim    (wmName =? "todo")             (customFloating $ W.RationalRect (1/6) (1/2) (2/3) (1/3))
    , NS "kmag"    "kmag"      (className =? "kmag")          (customFloating $ W.RationalRect 0.05 0.9 0.9 0.1)
    , NS "mpv"     "mpv"       (className =? "mpv")           (customFloating $ W.RationalRect 0.25 0.01 0.5 0.4)
    , NS "authy"   "authy"     (className =? "Authy Desktop") (customFloating $ W.RationalRect 0.25 0.01 0.5 0.4)
    ] where
        wmName = stringProperty "WM_NAME"
        namedVim = "namedVim.sh todo ~/todo/todo.txt"

-- Prompt theme
myXPConfig :: AConfig -> XPConfig
myXPConfig cfg = def
    { font              = cl_font cfg
    , bgColor           = cl_bg cfg
    , fgColor           = cl_fg0 cfg
    , bgHLight          = cl_bg cfg
    , fgHLight          = cl_lilly cfg
    , borderColor       = cl_lilly cfg
    , promptBorderWidth = 1
    , height            = fromIntegral $ cl_xpHeight cfg
    , position          = Bottom
    , historySize       = 100
    , historyFilter     = deleteConsecutive
    , autoComplete      = Nothing
    , completionKey     = (0,xK_Tab)
    , searchPredicate   = fuzzyMatch
    , sorter            = fuzzySort
    }

myCmds :: AConfig -> XConfig Layout -> [(String, X ())]
myCmds cfg conf =
    [ ("default-layout"      , setLayout $ XM.layoutHook conf)
    , ("recompile"           , spawn "xmonad --recompile; xmonad --restart;")
    , ("kill"                , kill1)
    , ("refresh"             , refresh)
    , ("quit-wm"             , io exitSuccess)
    , ("hotkeys"             , spawn "grep 'xK_' ~/coding/Xmonanza/xmonad/Main.hs | dmenu -l 42")
    , ("dunstctl-history-pop", spawn "dunstctl history-pop")
    , ("dunstctl-context"    , spawn "dunstctl context")
    , ("dunstctl-close"      , spawn "dunstctl close")
    , ("dunstctl-close-all"  , spawn "dunstctl close-all")
    , ("dunstctl-action"     , spawn "dunstctl action")
    , ("clip-to~/img.png"    , spawn $ cmdMaimSelect "~/img.png")
    , ("clip-to-feh"         , spawn $ cmdMaimSelect "/dev/stdout" ++ cmdPipeImgToClip ++ "&& xclip -selection clipboard -t image/png -o | feh -")
    , ("clip-to-server"      , spawn "clipImgToNixPiHttp")
    , ("setactivesink"       , spawn "~/bin/setActiveSink")
    , ("manPrompt"           , manPrompt (myXPConfig cfg))
    , ("optype"              , gsActionRunner (optypeCmds cfg) cfg)
    , ("lock"                , spawn "xscreensaver-command -lock")
    , ("hibernate"           , spawn "systemctl hibernate")
    ]

optypeCmds :: AConfig -> [([Char], X ())]
optypeCmds _ =
    [ ("ClipUsername"             , XUR.runInTerm alacrittyFloatingOpt "optype -c -u")
    , ("ClipPassword"             , XUR.runInTerm alacrittyFloatingOpt "optype -c -p")

    , ("Autofill"                 , spawn "optype")
    , ("TypePassword"             , spawn "optype -p")
    , ("TypeUsername"             , spawn "optype -u")
    ]

passCmds :: AConfig -> [([Char], X ())]
passCmds cfg =
    [ ("ClipUsername"             , passClipUsernamePrompt (myXPConfig cfg))
    , ("ClipPassword"             , passClipPasswordPrompt (myXPConfig cfg))

    , ("Autofill"                 , passAutofillPrompt (myXPConfig cfg))
    , ("TypePassword"             , passTypePasswordPrompt (myXPConfig cfg))
    , ("TypeUsername"             , passTypeUsernamePrompt (myXPConfig cfg))

    , ("Edit"                     , passEditPrompt (myXPConfig cfg))
    , ("Show"                     , passShowPrompt (myXPConfig cfg))

    , ("ClipOTP"                  , passClipOTPPrompt (myXPConfig cfg))
    , ("TypeOTP"                  , passTypeOTPPrompt (myXPConfig cfg))
    , ("AppendOTP"                , passAppendOTPPrompt (myXPConfig cfg))

    , ("GenerateNew"              , passGeneratePrompt "" (myXPConfig cfg))
    , ("GenerateNewNoSymbols"     , passGeneratePrompt "-n" (myXPConfig cfg))
    , ("GenerateExisting"         , passGeneratePrompt "--in-place" (myXPConfig cfg))
    , ("GenerateExistingNoSymbols", passGeneratePrompt "--in-place -n" (myXPConfig cfg))
    ]

data BrightnessCmd = Up | FullUp | Down | FullDown
cmdBrightness :: AConfig -> BrightnessCmd -> String
cmdBrightness cfg arg = hstNmCond cfg (HstNm "" "brightnessctl set " "xbacklight " "") ++ brightnessArg arg cfg

brightnessArg :: BrightnessCmd -> AConfig -> String
brightnessArg Up cfg       = hstNmCond cfg (HstNm "" "+5%"  "-inc 5%" "")
brightnessArg FullUp cfg   = hstNmCond cfg (HstNm "" "100%" "-set 100%" "")
brightnessArg Down cfg     = hstNmCond cfg (HstNm "" "5%-"  "-dec 5%" "")
brightnessArg FullDown cfg = hstNmCond cfg (HstNm "" "1"    "-set 0%" "")

cmdSetVolume :: String -> String
cmdSetVolume arg = "~/bin/setSinkVolumeDefault.sh " ++ arg
cmdMaimSelect :: String -> String
cmdMaimSelect out = "maim --select --hidecursor --format png " ++ out
cmdPipeImgToClip :: String
cmdPipeImgToClip = " | xclip -selection clipboard -t image/png -i"


selectWindow cfg = EM.selectWindow def 
  { EM.sKeys = EM.AnyKeys [xK_r, xK_s, xK_t, xK_n, xK_e, xK_i]
  , EM.bgCol = cl_bg cfg
  , EM.borderCol = cl_orange cfg
  , EM.txtCol = cl_fg0 cfg
  , EM.emFont = cl_font_very_big cfg
  } >>= (`whenJust` windows . W.focusWindow)
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: AConfig -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys cfg conf@XConfig {XM.modMask = modm} = M.fromList $
    [ ((modm.|.shiftMask,xK_Return), spawn $ XM.terminal conf)
    , ((0,                  xK_XF86AudioRaiseVolume ), spawn $ cmdSetVolume "+5%")
    , ((0,                  xK_XF86AudioLowerVolume ), spawn $ cmdSetVolume "-5%")
    , ((0,                  xK_XF86MonBrightnessDown), spawn $ cmdBrightness cfg Down)
    , ((modm,               xK_XF86MonBrightnessDown), spawn $ cmdBrightness cfg FullDown)
    , ((0,                  xK_XF86MonBrightnessUp  ), spawn $ cmdBrightness cfg Up)
    , ((modm,               xK_XF86MonBrightnessUp  ), spawn $ cmdBrightness cfg FullUp)
    , ((0,                  xK_Print                ), spawn $ cmdMaimSelect "/dev/stdout" ++ cmdPipeImgToClip)

    , ((modm,               xK_grave                ), gsActionRunner (passCmds cfg) cfg)
    , ((modm,               xK_q                    ), kill1)
    , ((modm,               xK_w                    ), spawn "~/bin/runner.sh")
    , ((modm,               xK_f                    ), selectWindow cfg)
    , ((modm .|. shiftMask, xK_f                    ), spawn "notify-send --urgency=low 'float: \n\
                                                       \ modm+{h,n,e,i} = resize\n\
                                                       \ modm+shift+{h,n,e,i} = resize(more)\n\
                                                       \ {h,n,e,i} = move\n\
                                                       \ shift+{h,n,e,i} = move(more)\n\'" >>
        (modalmap  . M.fromList $
             [ ((modm,            xK_h     ), withFocused (keysResizeWindow (-10,  0) (0,0)))
             , ((modm,            xK_n     ), withFocused (keysResizeWindow (  0, 10) (0,0)))
             , ((modm,            xK_e     ), withFocused (keysResizeWindow (  0,-10) (0,0)))
             , ((modm,            xK_i     ), withFocused (keysResizeWindow ( 10,  0) (0,0)))

             , ((modm .|. shiftMask, xK_h     ), withFocused (keysResizeWindow (-20,  0) (0,0)))
             , ((modm .|. shiftMask, xK_n     ), withFocused (keysResizeWindow (  0, 20) (0,0)))
             , ((modm .|. shiftMask, xK_e     ), withFocused (keysResizeWindow (  0,-20) (0,0)))
             , ((modm .|. shiftMask, xK_i     ), withFocused (keysResizeWindow ( 20,  0) (0,0)))

             , ((0,               xK_h     ), withFocused (keysMoveWindow   (-10,  0)))
             , ((0,               xK_n     ), withFocused (keysMoveWindow   (  0, 10)))
             , ((0,               xK_e     ), withFocused (keysMoveWindow   (  0,-10)))
             , ((0,               xK_i     ), withFocused (keysMoveWindow   ( 10,  0)))

             , ((shiftMask,       xK_h     ), withFocused (keysMoveWindow   (-20,  0)))
             , ((shiftMask,       xK_n     ), withFocused (keysMoveWindow   (  0, 20)))
             , ((shiftMask,       xK_e     ), withFocused (keysMoveWindow   (  0,-20)))
             , ((shiftMask,       xK_i     ), withFocused (keysMoveWindow   ( 20,  0)))
             ]
        )"submap: Float"
      )
    , ((modm,               xK_p        ), spawn "clipmenu")
    , ((modm,               xK_g        ), gsWindowGoto cfg)
    , ((modm,               xK_a        ), calculatorPrompt (myXPConfig cfg) )
    , ((modm,               xK_r        ), renameWorkspace (myXPConfig cfg))
    , ((modm .|. shiftMask, xK_r        ), resetWorkspaceNames)
    , ((modm,               xK_s        ), spawn "~/bin/openTerminalWithCurrentPwd.sh")
    , ((modm,               xK_t        ), promote)
    , ((modm,               xK_d        ), sendMessage NextLayout)
    , ((modm,               xK_z        ), withFocused $ windows . (`W.float` W.RationalRect 0 0 1 1))
    , ((modm,               xK_x        ), withFocused $ windows . W.sink)
    , ((modm,               xK_c        ), gsActionRunner (myCmds cfg conf) cfg)
    , ((modm,               xK_v        ), spawn "notify-send --urgency=low 'sublayout: \n\
                                                       \ modm+{h,n,e,i} = pullGroup {L,U,D,R}\n\
                                                       \ m = MergeAll\n\
                                                       \ u = UnMerge\n\
                                                       \ n = focusDown\n\
                                                       \ e = focusUp\n'" >>
        (modalmap  . M.fromList $
            [ ((modm, xK_h), sendMessage $ pullGroup L)
            , ((modm, xK_n), sendMessage $ pullGroup U)
            , ((modm, xK_e), sendMessage $ pullGroup D)
            , ((modm, xK_i), sendMessage $ pullGroup R)
            , ((0,    xK_m), withFocused (sendMessage . MergeAll))
            , ((0,    xK_u), withFocused (sendMessage . UnMerge))
            , ((0,    xK_n), onGroup W.focusDown')
            , ((0,    xK_e), onGroup W.focusUp')
            ]
        )"submap: sublayout"
      )
    , ((modm,               xK_b        ), sendMessage MD.ToggleStruts)
    , ((modm,               xK_j        ), spawn "~/bin/setxkbscript")
    , ((modm,               xK_y        ), spawn "~/bin/terminal.sh")
    , ((modm .|. shiftMask, xK_y        ), FN.toggleFloatAllNew >> FN.runLogHook)
    , ((modm,               xK_h        ), sendMessage Shrink)
    , ((modm .|. shiftMask, xK_h        ), sendMessage MirrorShrink)
    , ((modm,               xK_n        ), BRNG.focusDown)
    , ((modm .|. shiftMask, xK_n        ), windows W.swapDown  )
    , ((modm,               xK_e        ), BRNG.focusUp  )
    , ((modm .|. shiftMask, xK_e        ), windows W.swapUp    )
    , ((modm,               xK_i        ), sendMessage Expand)
    , ((modm .|. shiftMask, xK_i        ), sendMessage MirrorExpand)
    , ((modm,               xK_o        ), namedScratchpadAction scratchpads "todo")
    , ((modm,               xK_semicolon), namedScratchpadAction scratchpads "spotify")
    , ((modm,               xK_oslash   ), namedScratchpadAction scratchpads "spotify")
    , ((modm,               xK_equal    ), namedScratchpadAction scratchpads "authy")
    , ((modm,               xK_aring    ), namedScratchpadAction scratchpads "authy")
    , ((modm,               xK_period   ), windows W.focusMaster  )
    , ((modm,               xK_m        ), sendMessage (IncMasterN (-1)))
    , ((modm,               xK_comma    ), sendMessage (IncMasterN 1))
    , ((modm,               xK_slash    ), namedScratchpadAction scratchpads "mpv")
    , ((modm,               xK_Tab      ), nextWS)
    , ((modm .|. shiftMask, xK_Tab      ), prevWS)
    ]
    ++
    [((m .|. modm, k), f i)
      | (i, k) <- zip workspaceNames workspaceKeys
      , (f, m) <- [ (backAndForth, 0)
                  , (windows . W.shift, shiftMask)
                  , (swapWithCurrent,   controlMask)
                  , (windows . copy,    mod1Mask)
                  ]
    ]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_l, xK_u] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

resetWorkspaceNames :: X ()
resetWorkspaceNames = mapM_ (`setWorkspaceName` "") workspaceNames

workspaceNames :: [String]
workspaceNames = map show $ [1..9 :: Int] ++ [0]

workspaceKeys :: [KeySym]
workspaceKeys = [xK_1..xK_9] ++ [xK_0]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XM.modMask = modm} = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> XM.focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> XM.focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> XM.focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


------------------------------------------------------------------------
-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "qutebrowser" --> unfloat
    , className =? "TeamViewer"  --> unfloat
    , className =? floatingTermClass --> doFloat
    ]
    <+> FN.floatNextHook
    <+> namedScratchpadManageHook scratchpads
        where unfloat = ask >>= doF . W.sink




-- mouseHelpActions :: [(String, X ())]
-- mouseHelpActions = [
--     ("Cancel menu", return ())
--   , ("Kill"      , kill1)
--   , ("Promote"    , promote)
--   , ("Next layout", sendMessage NextLayout)
--   , ("Inc Master", sendMessage $ IncMasterN (-1))
--   , ("Dec Master", sendMessage $ IncMasterN 1)
--   , ("Expand", sendMessage Expand)
--   , ("Shrink", sendMessage Shrink)
--   , ("magnify", namedScratchpadAction scratchpads "kmag")
--   , ("copyToAll", windows copyToAll)
--   , ("killAllOtherCopies", killAllOtherCopies)
--   ]

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- myStartupHook :: AConfig -> X ()
-- myStartupHook = gsWithWindows mouseHelpActions

--screenCornerStuff c = c 
                        -- { handleEventHook = handleEventHook c <+> screenCornerEventHook
                        -- , startupHook     = addScreenCorner SCUpperRight $ startupHook c
                        -- , layoutHook      = screenCornerLayoutHook $ layoutHook c
                        -- }

mySB :: FilePath -> AConfig -> SB.StatusBarConfig
mySB xmobarExePath cfg = (SB.statusBarProp xmobarExePath (workspaceNamesPP def
    { SBPP.ppCurrent = fgXmobarColor (cl_lilly cfg) . formatWs
    , SBPP.ppHidden  = formatWs
    , SBPP.ppTitle   = fgXmobarColor (cl_lilly cfg)
    , SBPP.ppTitleSanitize = Prelude.filter (`elem` xmobarTitleAllowedChars) . SBPP.xmobarStrip
    , SBPP.ppUrgent  = fgXmobarColor (cl_aqua  cfg) . formatWs
    , SBPP.ppOrder   = toOrdr
    , SBPP.ppSep     = " | "
    , SBPP.ppVisible = fgXmobarColor (cl_green cfg)
    , SBPP.ppExtras = [FN.willFloatAllNewPP (fgXmobarColor (cl_red cfg) . ("FloatNext: " ++)), fmap (fgXmobarColor (cl_aqua cfg)) . getModeName <$> (XS.get :: X ModeName)]
    })) { SB.sbCleanupHook = SB.killAllStatusBars, SB.sbStartupHook = SB.killAllStatusBars >> SB.spawnStatusBar xmobarExePath }
  where
    fgXmobarColor color = SBPP.xmobarColor color ""
    toOrdr (wsNames:_layoutName:windowTitle:xtras:_) = [scrollableWsNames wsNames,xtras,windowTitle]
    toOrdr (wsNames:_layoutName:windowTitle:_) = [scrollableWsNames wsNames,windowTitle]
    toOrdr _ = ["wtf something weird"]
    xmobarTitleAllowedChars = [' '..'~']
    scrollableWsNames :: String -> String
    scrollableWsNames wsNames = SBPP.xmobarAction "xdotool key Super_L+Shift+Tab" "5" (SBPP.xmobarAction "xdotool key Super_L+Tab" "4" wsNames)
    -- hide NSP ws rest of ws make clickable with xdotool
    formatWs "NSP"  = ""
    formatWs wsName = SBPP.xmobarAction ("xdotool key Super_L+" ++ wsIdx) "1" wsName
      where wsIdx = takeWhile (/=':') $ SBPP.xmobarStrip wsName

main :: IO ()
main = do
  xmonadExePath <- SE.getExecutablePath
  let xmobarExePath = SF.replaceFileName xmonadExePath "xmobar"
  cfg <- getConfig
  xmonad . 
      SB.withSB (mySB xmobarExePath cfg) . 
      workspaceNamesEwmh .
      EWMH.ewmhFullscreen .
      EWMH.ewmh .
      applyRefocusLastHooks .
      withUrgencyHook NoUrgencyHook .
      MD.docks $ defaults cfg

applyRefocusLastHooks :: XConfig l -> XConfig (ModifiedLayout RefocusLastLayoutHook l)
applyRefocusLastHooks c = c { handleEventHook = handleEventHook c <+> refocusLastWhen isFloat
                            , layoutHook      = refocusLastLayoutHook $ layoutHook c
                            --, logHook         = logHook c <+> refocusLastLogHook
                            }

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
defaults cfg = def {
        terminal           = "alacritty",
        focusFollowsMouse  = False,
        clickJustFocuses   = False,
        borderWidth        = 5,
        modMask            = mod4Mask,
        XM.workspaces      = workspaceNames,
        normalBorderColor  = cl_bg cfg,
        focusedBorderColor = cl_orange cfg,
        keys               = myKeys cfg,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout cfg,
        manageHook         = myManageHook,
        handleEventHook    = Hacks.trayerAboveXmobarEventHook,
        -- handleEventHook    = myEventHook,
        logHook            = workspaceHistoryHook
        -- startupHook        = myStartupHook cfg
    }

