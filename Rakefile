require 'fileutils'

def info(msg)
    print " [ \033[00;34m..\033[0m ] #{msg}"
end

def success(msg)
    print "\r\033[2K [ \033[00;32mOK\033[0m ] #{msg}\n"
end

def failed(msg)
    print "\r\033[2K [ \033[0;31mFAIL\033[0m ] #{msg}\n"
end

def bold(msg)
    "\033[1m#{msg}\033[0m"
end

def lnif(src_name, dst_name)
    src = File.dirname(__FILE__) + "/#{src_name}"
    dst = ENV['HOME'] + "/.#{dst_name}"
    link = "#{src} -> #{dst}"
    output = bold("Linking: ") + link

    info output
    begin
        FileUtils.ln_s src, dst
    rescue Errno::EEXIST
        failed output
        puts "\t | #{dst} alredy exists\n"
        return
    end
    success output
end

def action_msg(action, aname)
    output = bold(aname) + ": " + action
    info output
    `#{action}`
    unless $?.success?
        failed output
        return false
    end
    success output
    return true
end

def github_clone(what, where)
    clone = "https://github.com/#{what} #{where}"
    return action_msg "git clone -q #{clone} 2> /dev/null", 'Git clone'
end

def vim_install
    vimrc = ENV['HOME'] + "/.vimrc"
    vundle = ENV['HOME'] + "/.vim/bundle/Vundle.vim"
    solarized = ENV['HOME'] + "/.vim/bundle/vim-colors-solarized"

    github_clone 'altercation/vim-colors-solarized.git', solarized
    github_clone 'gmarik/vundle', vundle
    github_clone 'powerline/fonts.git', '/tmp/powerline_fonts'
    action_msg 'fc-cache -vf', 'Cache fonts'
    lnif 'vimrc', 'vimrc'
    action_msg 'vim +PluginInstall +qall 2&> /dev/null', 'Installing vim plugins'
end

task :default => [:xinitrc, :bash, :xresources, :vim, :ncmpcpp, :mpd, :mpdscribble, :xmonad]

task :bash do
    bashrc = ENV['HOME'] + "/.bashrc"
    lnif 'bash', 'bash'
    lnif 'bash/bashrc', 'bashrc'
    lnif 'bash/dircolors', 'dircolors'
    action_msg 'source ~/.bashrc', 'Reload bash'
end

task :git do
    lnif 'gitconfig', 'gitconfig'
end

task :mpd do
    lnif 'config/mpd/mpd.conf', 'config/mpd/mpd.conf'
end

task :mpdscribble do
    lnif 'config/systemd/user/mpdscribble.service', 'config/systemd/user/mpdscribble.service'
    lnif 'mpdscribble/mpdscribble.conf', 'mpdscribble/mpdscribble.conf'
end

task :ncmpcpp do
    lnif 'ncmpcpp', 'ncmpcpp'
end

task :vim do
    vim_install
end

task :xinitrc do
    lnif 'xinitrc', 'xinitrc'
end

task :xmonad do
    lnif 'xmonad', 'xmonad'
    action_msg 'xmonad --recompile', 'Compiling XMonad'
end

task :xresources do
    lnif 'Xresources', 'Xresources'
end

