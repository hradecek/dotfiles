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
    "\033[1m#{msg}:\033[0m"
end

def lnif(src_name, dst_name)
    src = File.dirname(__FILE__) + "/#{src_name}"
    dst = ENV['HOME'] + "/.#{dst_name}"
    link = "#{src} -> #{dst}"
    output = bold "Linking: " + link

    info output
    begin
        FileUtils.ln_s src, dst
    rescue Errno::EEXIST
        failed output
        puts "\t #{dst} alredy exists"
        return
    end
    success output
end

def install_if(action, aname, msg)
    output = bold(aname) + ": " + msg
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
    return install_if "git clone -q #{clone}", 'Git clone', clone
end

def vim_install
    vimrc = ENV['HOME'] + "/.vimrc"
    vundle = ENV['HOME'] + "/.vim/bundle/Vundle.vim"

    unless github_clone 'gmarik/vundle', vundle
      return
    end

    if File.exists?(vundle)
      unless install_if 'vim +PluginInstall +qall 2&> /dev/null', 'Installing vim plugins', 'vim +PluginInstall +qall'
        return
      end
    end

    if github_clone 'powerline/fonts.git', '/tmp/powerline_fonts'
        install_if '/tmp/powerline_fonts/install.sh', 'Installing powerline fonts', ''
    install_if 'fc-cache -vf', '', ''
    end

    if File.exists?(vimrc)
        File.rename(vimrc, vimrc + ".old")
        info "\033[1mBackup\033[0m ~/.vimrc renamed to ~/.vimrc.old"
    end
    lnif 'vimrc', 'vimrc'
end

task :default => [:xinitrc, :bash, :xresources, :vim, :ncmpcpp]

task :bash do
    bashrc = ENV['HOME'] + "/.bashrc"
    if File.exists?(bashrc)
        File.rename(bashrc, bashrc + ".old")
        info "\033[1mBackup\033[0m ~/.bashrc renamed to ~/.bashrc.old"
    end
    lnif 'bash', 'bash'
    lnif 'bash/bashrc', 'bashrc'
    lnif 'bash/dircolors', 'dircolors'
    `source ~/.bashrc`
end

task :xmonad do
    lnif 'xmonad', 'xmonad'
    install_if 'xmonad --recompile', 'Compiling XMonad', 'PRD'
end

task :xresources do
    lnif 'Xresources', 'Xresources'
end

task :ncmpcpp do
    lnif 'ncmpcpp', 'ncmpcpp'
end

task :xinitrc do
    lnif 'xinitrc', 'xinitrc'
end

task :mpd do
    lnif 'config/mpd/mpd.conf', 'config/mpd/mpd.conf'
end

task :git do
    lnif 'gitconfig', 'gitconfig'
end

task :mpdscribble do
    lnif 'mpdscribble/mpdscribble.conf', 'mpdscribble/mpdscribble.conf'
end

task :systemd do
    lnif 'config/systemd/user/mpd'
end

task :vim do
    vim_install
end
