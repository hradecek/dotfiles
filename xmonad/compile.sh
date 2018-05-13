#!/bin/bash
ghc --make xmonad.hs -outputdir target -i -imodules -dynamic -fforce-recomp -main-is main -o xmonad-$(uname -m)-linux
