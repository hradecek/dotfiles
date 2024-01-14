#==============================================================================#
#                                  MAKEFILE                                    #
#                                                                              #
# @file:   $HOME/dotfiles/Makefile                                             #
# @author: hradecek <ivohradek@gmail.com>                                      #
#==============================================================================#
SYMLINK=ln -s
FCCACHE=fc-cache -vf
GITCLONE=/usr/bin/git clone -q
CONFIG_HOME=$(HOME)/.config

# Alacritty
ALACRITTY_DOTFILES=$(PWD)/alacritty
ALACRITTY_HOME=$(CONFIG_HOME)/alacritty

# Waybar
WAYBAR_DOTFILES=$(PWD)/waybar
WAYBAR_HOME=$(CONFIG_HOME)/waybar

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

all: vim

alacritty:
	@echo 'Installing Alacritty (terminal)'
	$(SYMLINK) '$(ALACRITTY_DOTFILES)' '$(ALACRITY_HOME)'
	# $(SYMLINK) '$(ALACRITTY_DOTFILES)/colors.toml' '$(ALACRITTY_HOME)/colors.toml'
	# $(SYMLINK) '$(ALACRITTY_DOTFILES)/alacritty.toml' '$(ALACRITTY_HOME)/alacritty.toml'

alacritty-clean:
	@echo 'Removing Alacritty'
	rm -rf '$(ALACRITTY_HOME)'

zsh:
	@echo 'Installing ZSH'
	$(SYMLINK) '$(PWD)/shell/zsh/zshrc.zsh' '$(HOME)/.zshrc'

spaceship-prompt: zsh
	@echo 'Installing Spaceship prompt'
	$(SYMLINK) '$(PWD)/shell/spaceship/spaceshiprc.zsh' '$(HOME)/.spaceshiprc.zsh'

waybar:
	@echo 'Installing Waybar'
	$(SYMLINK) '$(WAYBAR_DOTFILES)' '$(WAYBAR_HOME)'

waybar-clean:
	@echo 'Removing Waybar'
	rm -rf '$(WAYBAR_HOME)'

mpd:
	mkdir '$(CONFIG_HOME)/mpd'
	mkdir '$(HOME)/Music'
	$(SYMLINK) '$(PWD)/$(MPD_CONF)' '$(CONFIG_HOME)/$(MPD_CONF)'
	mpc update

ncmpcpp:
	mkdir '$(CONFIG_HOME)/ncmpcpp'
	$(SYMLINK) '$(PWD)/$(NCMPCPP_CONFIG)' '$(CONFIG_HOME)/$(NCMPCPP_CONFIG)'
	$(SYMLINK) '$(PWD)/$(NCMPCPP_BINDINGS)' '$(CONFIG_HOME)/$(NCMPCPP_BINDINGS)'

vim:
	@echo 'Installing vim'
	mkdir $(VIMSWAP)
	$(GITCLONE) https://github.com/altercation/vim-colors-solarized.git $(SOLARIZED)
	$(GITCLONE) https://github.com/gmarik/vundle $(VUNDLE)
	$(GITCLONE) https://github.com/powerline/fonts.git /tmp/powerline_fonts
	$(FCCACHE)
	$(SYMLINK) '$(PWD)/vimrc' '$(HOME)/.vimrc'
	vim +PluginInstall +qall

.PHONY: alacritty waybar
