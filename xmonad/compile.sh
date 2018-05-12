#!/bin/bash
ghc --make xmonad.hs -i -imodules -dynamic -fforce-recomp -main-is main -o xmonad-$(uname -m)-linux
