# XmoFlake
add input:
```
inputs.xmoflake.url = path:Xmoflake; # recommend cloning repo, and using path, so you can more easily change stuff
```
(If xmoflake is a git repo, you need to use flag submodules=1 like so `sudo nixos-rebuild switch --flake '.?submodules=1#'`)  
add in ur nixos-config like:  
```
{ nixpkgs.overlays = [ xmoflake.overlay ]; }
```
Can for instance be used like this in home-manager:
```
xsession = {
    enable = true;
    windowManager.command = "systemd-cat --identifier=xmonad ${pkgs.xmoflake}/bin/xmonad";
};
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
To have nixos launch the `xsession.windowManager.command` we have defined, you can add this in nixos-config:
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
