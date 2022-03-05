# XmoFlake
add in ur flake like:
```
{ nixpkgs.overlays = [ xmoflake.overlay ]; }
```
`xmonad` launches `xmobar` from `$PATH` so add it to home/system packages.  
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
Then in-place-recompilation will work for xmonad. (but not xmobar yet)
