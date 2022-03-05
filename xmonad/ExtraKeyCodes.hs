module ExtraKeyCodes (xK_XF86AudioRaiseVolume, xK_XF86AudioLowerVolume, xK_XF86MonBrightnessUp, xK_XF86MonBrightnessDown) where

import XMonad.Config.Prime (KeySym)
xK_XF86AudioRaiseVolume :: KeySym
xK_XF86AudioRaiseVolume = 0x1008ff13
xK_XF86AudioLowerVolume :: KeySym
xK_XF86AudioLowerVolume = 0x1008ff11

xK_XF86MonBrightnessUp :: KeySym
xK_XF86MonBrightnessUp = 0x1008ff02
xK_XF86MonBrightnessDown :: KeySym
xK_XF86MonBrightnessDown = 0x1008ff03
