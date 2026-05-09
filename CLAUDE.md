# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
nix develop          # Enter dev shell with HLS, hlint, hpack, and all dependencies
nix build            # Build the xmonad executable
hpack                # Regenerate .cabal from package.yaml (required for HLS hints)
```

Integration rebuild (when used as submodule in nixos-config):
```bash
sudo nixos-rebuild switch --flake '.?submodules=1#'
```

## Host-Aware Configuration

The system detects hostname and applies different configurations. Use the `HstNm` record type:

```haskell
hstNmCond :: AConfig -> HstNm a -> a

HstNm { hst_hanstop   -- Laptop
      , hst_nimbus2k  -- Dev laptop (supports compact mode)
      , hst_hogwarts  -- Desktop workstation
      , hst_other     -- Fallback
      }
```

## Configuration Loading

`AConfig.getConfig` loads from `~/.config/xmoflake.json` with hardcoded fallback defaults. See `shared/AConfig.hs` for all fields.

## External Dependencies

Commands used by keybindings: `calc`, `brightnessctl`/`xbacklight`, `volume-fast.sh` (wraps `wpctl`, `canberra-gtk-play`, `qs ipc`), `playerctl`, `maim`, `xclip`, `pass`, `optype`, `qs ipc` (notifications), `setxkbmap`
