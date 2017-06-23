# General variables
GITCLONE=/usr/bin/git clone -q
SYMLINK=ln -s
FCCACHE=fc-cache -vf

# Vim configurations
VIMRC='$(HOME)/.vimrc'
VUNDLE='$(HOME)/.vim/bundle/Vundle.vim'
SOLARIZED='$(HOME)/.vim/bundle/vim-colors-solarized'

all:	xmonad vim
xmonad:
	$(SYMLINK) '$(PWD)/xmonad' '$(HOME)/.xmonad'
vim:
	@echo 'Installing vim'
	$(GITCLONE) https://github.com/altercation/vim-colors-solarized.git $(SOLARIZED)
	$(GITCLONE) https://github.com/gmarik/vundle $(VUNDLE)
	$(GITCLONE) https://github.com/powerline/fonts.git /tmp/powerline_fonts
	$(FCCACHE)
	$(SYMLINK) '$(PWD)/vimrc' '$(HOME)/.vimrc'
	vim +PluginInstall +qall

.PHONY: clean

clean:
	rm -rf /tmp/powerline_fonts
