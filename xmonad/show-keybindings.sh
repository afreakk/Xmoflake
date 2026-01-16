#!/usr/bin/env nix-shell
#! nix-shell -i bash -p yad
# XMonad Keybindings Help
# This script displays all keybindings in a floating window

KEYBINDINGS='
GENERAL
--------------------------------------------------------------------------------
  Mod+F1             Show this help
  Mod+Shift+Enter    Spawn terminal
  Mod+q              Kill focused window
  Mod+c              Commands grid select
  Mod+w              Runner (dmenu/rofi)
  Mod+d              Next layout
  Mod+.              Focus master window

WINDOW NAVIGATION
--------------------------------------------------------------------------------
  Mod+n              Focus down (next window)
  Mod+e              Focus up (previous window)
  Mod+u              Focus urgent window
  Mod+f              EasyMotion window select
  Mod+g              Grid select window goto

WINDOW MANAGEMENT
--------------------------------------------------------------------------------
  Mod+t              Promote window to master
  Mod+Shift+n        Swap window down
  Mod+Shift+e        Swap window up
  Mod+z              Float fullscreen
  Mod+x              Sink floating window
  Mod+,              Increase master count
  Mod+m              Decrease master count

RESIZE
--------------------------------------------------------------------------------
  Mod+h              Shrink master
  Mod+i              Expand master
  Mod+Shift+h        Mirror shrink
  Mod+Shift+i        Mirror expand

WORKSPACES
--------------------------------------------------------------------------------
  Mod+[1-9,0]        Switch to workspace
  Mod+Shift+[1-9,0]  Move window to workspace
  Mod+Ctrl+[1-9,0]   Swap with workspace
  Mod+Alt+[1-9,0]    Copy window to workspace
  Mod+Tab            Next workspace
  Mod+Shift+Tab      Previous workspace
  Mod+r              Rename workspace
  Mod+Shift+r        Reset workspace names

SCRATCHPADS
--------------------------------------------------------------------------------
  Mod+o              Todo scratchpad
  Mod+;              Spotify scratchpad
  Mod+=              Authy scratchpad
  Mod+/              MPV scratchpad

MODAL MODES
--------------------------------------------------------------------------------
  Mod+Shift+f        Enter float manipulation mode
  Mod+v              Enter sublayout manipulation mode

  Float Mode (after Mod+Shift+f):
    h/n/e/i          Move window (10px)
    Shift+h/n/e/i    Move window (20px)
    Mod+h/n/e/i      Resize window (10px)
    Mod+Shift+h/n/e/i Resize window (20px)
    Escape           Exit mode

  SubLayout Mode (after Mod+v):
    Mod+h/n/e/i      Pull group direction
    m / Mod+m        Merge all
    u / Mod+u        Unmerge
    n/e              Focus within group
    Escape           Exit mode

PROMPTS & TOOLS
--------------------------------------------------------------------------------
  Mod+`              Pass password manager
  Mod+a              Calculator prompt
  Mod+p              Clipmenu
  Mod+Shift+o        Todofi

APPLICATIONS
--------------------------------------------------------------------------------
  Mod+y              Terminal
  Mod+Shift+y        Toggle float-all-new
  Mod+s              Terminal in current PWD

MEDIA KEYS
--------------------------------------------------------------------------------
  XF86AudioRaiseVol  Volume up
  XF86AudioLowerVol  Volume down
  XF86AudioMute      Mute toggle
  XF86AudioPlay      Play/pause
  XF86AudioPrev      Previous track
  XF86AudioNext      Next track

BRIGHTNESS
--------------------------------------------------------------------------------
  XF86BrightnessUp   Brightness +5%
  XF86BrightnessDn   Brightness -5%
  Mod+BrightnessUp   Brightness 100%
  Mod+BrightnessDn   Brightness minimum

SCREENSHOT
--------------------------------------------------------------------------------
  Print              Screenshot selection to clipboard

NOTIFICATIONS
--------------------------------------------------------------------------------
  Mod+b              Close notification

'

# Display with yad (provided by nix-shell)
# Use class=XmonadHelp to match the named scratchpad
echo "$KEYBINDINGS" | yad --text-info \
    --title="XMonad Keybindings" \
    --class="XmonadHelp" \
    --width=550 \
    --height=700 \
    --fontname="monospace 10" \
    --button="Close:0"
