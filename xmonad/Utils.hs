module Utils (stdinToClip, floatingTermClass, alacrittyFloatingOpt) where

stdinToClip = "xclip -in -selection primary -f -r | xclip -in -selection clipboard -r"

floatingTermClass = "FloatingTerm"

alacrittyFloatingOpt = "--class "++floatingTermClass++","++floatingTermClass
