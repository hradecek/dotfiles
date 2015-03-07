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

def lnif(target)
    src = File.dirname(__FILE__) + "/#{target}"
    dst = ENV['HOME'] + "/.#{target}"
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

def vim_install
    vundle = ENV['HOME'] + "/.vim/bundle/vundle"
    clone = "https://github.com/gmarik/vundle #{vundle}"

    unless File.exists?(vundle)
        # FileUtils.mkdir_(vim)

        info "\033[1mGit cloning:\033[0m #{clone}"
        `git clone #{clone} 2> /dev/null`
        if $?.success?
            success "\033[1mGit clone:\033[0m #{clone}"
        else
            fail "\033[1mGit clone:\033[0m #{clone}"
            return
        end

        info "\033[1mInstalling vim plugins:\033[0m"
        `vim +PluginInstall +qall 2&> /dev/null`
        if $?.success?
            success "\033[1mInstalling vim plugins:\033[0m"
        else
            fail "\033[1mInstalling vim plugins:\033[0m"
            return
        end
    end
end

task :default => [:xinitrc, :bash, :vim, :ncmpcpp]

task :bash do
    lnif 'bash'
    lnif 'bashrc'
#    `. ~/.bashrc`
end

task :ncmpcpp do
    lnif 'ncmpcpp'
end

task :xinitrc do
    lnif 'xinitrc'
end

task :vim do
    vim_install
end
