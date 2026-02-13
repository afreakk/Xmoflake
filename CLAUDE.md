# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
nix develop          # Enter dev shell with HLS, hlint, hpack, and all dependencies
nix build            # Build the xmonad executable
hpack                # Regenerate .cabal from package.yaml (required for HLS hints)
```

Built executable: `./result/bin/xmonad`

Integration rebuild (when used as submodule in nixos-config):
```bash
sudo nixos-rebuild switch --flake '.?submodules=1#'
```

## Architecture

XmoFlake is a Haskell-based XMonad window manager configuration using Nix Flakes.

### Module Structure

```
xmonad/           # Window manager
├── Main.hs       # Keybindings, scratchpads, window rules
├── LayoutHook.hs # Layout definitions (ResizableTile, tabbed, spacing)
├── PassFork.hs   # Pass password manager integration with multiple prompts
├── GridSelects.hs # Grid-based action runner and window selector
├── Calculator.hs # Calculator prompt using `calc` command
├── ExtraKeyCodes.hs # XF86 multimedia key definitions
└── Utils.hs      # Floating terminal class, clipboard utils

shared/
└── AConfig.hs    # Config: hostname detection, colors, fonts, DPI
```

### Host-Aware Configuration

The system detects hostname and applies different configurations. Use the `HstNm` record type:

```haskell
hstNmCond :: AConfig -> HstNm a -> a

HstNm { hst_hanstop   -- Laptop
      , hst_nimbus2k  -- Dev laptop (supports compact mode)
      , hst_hogwarts  -- Desktop workstation
      , hst_other     -- Fallback
      }
```

### Configuration Loading

`AConfig.getConfig` loads from `~/.config/xmoflake.json` with hardcoded fallback defaults.

Key AConfig fields:
- `cl_bg`, `cl_fg`, `cl_accent`, `cl_alert`, `cl_finecolor` - Theme colors
- `cl_font`, `cl_font_pango`, `cl_font_big`, `cl_font_very_big` - Fonts
- `cl_hostName` - Detected hostname enum (Hanstop | Nimbus2k | Hogwarts | Other)
- `cl_compact` - Toggle compact bar mode (for nimbus2k mobile display)
- `cl_barHeight`, `cl_tabHeight`, `cl_xpHeight`, `cl_dpi` - Display settings
- `cl_gsCellWidth`, `cl_gsCellHeight` (and big variants) - Grid select dimensions

### XMonad Key Features

- 10 workspaces (1-9, 0) with persistent naming
- 5 named scratchpads: spotify, todo, kmag, mpv, authy
- Modal manipulation modes: float manipulation (`M-S-f`), sublayout manipulation (`M-v`)
- Grid-based action runner with fuzzy search (`M-g`)
- Pass integration with OTP and autofill support
- Brightness control (host-aware: brightnessctl vs xbacklight)

### Data Flow

```
startup → getConfig() reads ~/.config/xmoflake.json
        → Falls back to hardcoded defaults per hostname
        → AConfig used for layouts, prompts, colors, grid selects
```

## External Dependencies

Commands used by keybindings: `calc`, `brightnessctl`/`xbacklight`, `i3-volume`, `playerctl`, `maim`, `xclip`, `pass`, `optype`, `qs ipc` (notifications), `setxkbmap`
