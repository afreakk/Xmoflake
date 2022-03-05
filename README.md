# XmoFlake
add in ur flake like:
```
{ nixpkgs.overlays = [ xmoflake.overlay ]; }
```
Can for instance be used like this in home-manager:
```
xsession.windowManager.command = "systemd-cat --identifier=xmonad ${pkgs.xmoflake}/bin/xmonad";
home.packages = [ pkgs.xmoflake ];
home.file.xmonadBuildScript = {
  target = ".xmonad/build";
  executable = true;
  text = ''
    #!/usr/bin/env bash
    set -e

    XmoflakeDir="$HOME/coding/Xmoflake"

    cd "$XmoflakeDir"
    nix build

    ln -s -f -T "$XmoflakeDir/result/bin/xmonad" "$1"
  '';
```
