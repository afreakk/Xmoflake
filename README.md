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
    # CHANGE THIS TO MATCH THE ACTUAL LOCATION OF XMOFLAKE REPO !!
    XmoflakeDir="$HOME/coding/Xmoflake"

    cd "$XmoflakeDir"
    nix build

    ln -s -f -T "$XmoflakeDir/result/bin/xmonad" "$1"
  '';
```
Then in-place-recompilation will work for xmonad and xmobar!  
And to have nixos launch the `xsession.windowManager.command` we have defined, you can add this in nixos-config:
```
  services.xserver = {
    #from https://discourse.nixos.org/t/opening-i3-from-home-manager-automatically/4849/8
    windowManager.session = [{
      name = "home-manager";
      start = ''
        ${pkgs.runtimeShell} $HOME/.xsession &
        waitPID=$!
      '';
    }];
    displayManager.lightdm.enable = true;
    enable = true;
    layout = "us";
  };
```
If there is a less hacky way of launching home-manager's `windowManager.command`, please tell me :)
