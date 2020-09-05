#===============================================================================
# Makefile
#===============================================================================
SYMLINK=ln -s
FCCACHE=fc-cache -vf
GITCLONE=/usr/bin/git clone -q
XDG_CONFIG_HOME=$(HOME)/.config

# mpd & ncmpcpp
MPC=mpc
MPD_CONF=mpd/mpd.conf
NCMPCPP_CONFIG=ncmpcpp/config
NCMPCPP_BINDINGS=ncmpcpp/bindings

# vim configurations
VIMRC='$(HOME)/.vimrc'
VIMSWAP='$(HOME)/.vimswap'
VUNDLE='$(HOME)/.vim/bundle/Vundle.vim'
SOLARIZED='$(HOME)/.vim/bundle/vim-colors-solarized'

all: xmonad vim

mpd:
	mkdir '$(XDG_CONFIG_HOME)/mpd'
	mkdir '$(HOME)/Music'
	$(SYMLINK) '$(PWD)/$(MPD_CONF)' '$(XDG_CONFIG_HOME)/$(MPD_CONF)'
	mpc update

ncmpcpp:
	mkdir '$(XDG_CONFIG_HOME)/ncmpcpp'
	$(SYMLINK) '$(PWD)/$(NCMPCPP_CONFIG)' '$(XDG_CONFIG_HOME)/$(NCMPCPP_CONFIG)'
	$(SYMLINK) '$(PWD)/$(NCMPCPP_BINDINGS)' '$(XDG_CONFIG_HOME)/$(NCMPCPP_BINDINGS)'

polybar:
	$(SYMLINK) '$(PWD)/polybar' '$(XDG_CONFIG_HOME)/polybar'

termite: FORCE
	$(SYMLINK) '$(PWD)/termite' '$(XDG_CONFIG_HOME)/termite'

xmonad:
	$(SYMLINK) '$(PWD)/xmonad' '$(HOME)/.xmonad'

vim:
	@echo 'Installing vim'
	mkdir $(VIMSWAP)
	$(GITCLONE) https://github.com/altercation/vim-colors-solarized.git $(SOLARIZED)
	$(GITCLONE) https://github.com/gmarik/vundle $(VUNDLE)
	$(GITCLONE) https://github.com/powerline/fonts.git /tmp/powerline_fonts
	$(FCCACHE)
	$(SYMLINK) '$(PWD)/vimrc' '$(HOME)/.vimrc'
	vim +PluginInstall +qall

FORCE: ;

.PHONY: clean

clean:
	rm -rf /tmp/powerline_fonts

