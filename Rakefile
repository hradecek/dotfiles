require 'fileutils'

def info(msg)
    print " [ \033[00;34m..\033[0m ] #{msg}"
end

def success(msg)
    print "\r\033[2K [ \033[00;32mOK\033[0m ] #{msg}\n"
end

def fail(msg)
    print "\r\033[2K [ \033[0;31mFAIL\033[0m ] #{msg}\n"
end

def lnif(src_name, dst_name)
    src = File.dirname(__FILE__) + "/#{src_name}"
    dst = ENV['HOME'] + "/.#{dst_name}"
    link = "#{src} -> #{dst}"

    info "\033[1mLinking:\033[0m #{link}"
    begin
        FileUtils.ln_s src, dst
    rescue Errno::EEXIST
        fail "\033[1mLinking:\033[0m #{link}"
        puts "\t #{dst} alredy exists"
        return
    end
    success "Linked: #{link}"
end

def install_if(action, aname, msg)
    output = "\033[1m#{aname}\033[0m #{msg}"
    info output
    `#{action}`
    puts action
    if $?.success?
        success output
    else
        fail output
        return false
    end
    return true
end

def github_clone(what, where)
    clone = "https://github.com/#{what} #{where}"
    return install_if "git clone #{clone}", 'Git clone', clone
end

def vim_install
    vundle = ENV['HOME'] + "/.vim/bundle/Vundle.vim"
    lnif 'vimrc', 'vimrc'

    unless File.exists?(vundle)
        install_if 'vim +PluginInstall +qall 2&> /dev/null', 'Installing vim plugins', '' if github_clone 'gmarik/vundle', vundle
    end

    if github_clone 'powerline/fonts.git', '/tmp/powerline_fonts'
        install_if '/tmp/powerline_fonts/install.sh', 'Installing powerline fonts', ''
        `fc-cache -vf`
    end
end

task :default => [:xinitrc, :bash, :xresources, :vim, :ncmpcpp]

task :bash do
    lnif 'bash', 'bash'
    lnif 'bash/bashrc', 'bashrc'
    `. ~/.bashrc`
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

# task :systemd do
#     lnif 'config/systemd/user/mpd
# end

task :vim do
    vim_install
end
