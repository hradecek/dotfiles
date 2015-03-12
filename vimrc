"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" File: ~/.vimrc                                                              "
" Author: Ivo Hradek <ivohradek@gmail.com>                                    "
" Description: Vim configuration file                                         "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible
set encoding=utf-8

let mapleader=","

" let g:solarized_termcolors=256
let g:airline_theme="murmur"
let g:airline_powerline_fonts = 1
let g:ack_autoclose = 1
let g:ackhighlight = 1
" let g:ack_use_dispatch = 1

" Basic settings
set list
set wrap
set ruler
set number
set confirm
set showcmd
set ttyfast
set hlsearch
set showmode
set undofile
set wildmenu
set expandtab
set incsearch
set showmatch
set smartcase
set autoindent
set ignorecase
set nolinebreak
set smartindent
set nojoinspaces
set relativenumber

set t_Co=256
set tabstop=4
set history=50
set backspace=2
set scrolloff=3
set showbreak=+
set laststatus=2
set shiftwidth=4
set numberwidth=3
set softtabstop=4
set wildmode=full
set timeoutlen=250
set wildchar=<Tab>
set viminfo='50,\"500
set matchpairs=(:),[:],{:},<:>
set rtp+=~/.vim/bundle/Vundle.vim
set listchars=eol:$,tab:>\ ,trail:.,extends:+,precedes:-

" Mappings
" nnoremap ; :
inoremap jj <Esc>

map <F3>  :cp<cr>
map <F4>  :cn<cr>
map <F9>  :w<cr>:make<cr>
map <F10> :cl<cr><cr>

" Toggle paste mode
nmap <silent> <F4> :set invpaste<CR>:set paste?<CR>
imap <silent> <F4> <ESC>:set invpaste<CR>:set paste?<CR>

" format the entire file
nnoremap <leader>fef :normal! gg=G``<CR>

" upper/lower word
nmap <leader>u mQviwU`Q
nmap <leader>l mQviwu`Q

" Some helpers to edit mode
" http://vimcasts.org/e/14
nmap <leader>ew :e <C-R>=expand('%:h').'/'<cr>
nmap <leader>es :sp <C-R>=expand('%:h').'/'<cr>
nmap <leader>ev :vsp <C-R>=expand('%:h').'/'<cr>
nmap <leader>et :tabe <C-R>=expand('%:h').'/'<cr>

" Toggle hlsearch with <leader>hs
nmap <leader>hs :set hlsearch! hlsearch?<CR>

" Adjust viewports to the same size
map <Leader>= <C-w>=

nnoremap <leader><space> :noh<cr>
nnoremap <tab> %
vnoremap <tab> %

map <C-q> :qa<cr>
imap <C-q> <Esc>:qa<cr>

map <C-s> :w<cr>
imap <C-s> <Esc>:w<cr>i

imap } }<Esc>=%``a

imap <C-h> <C-o>h
imap <C-j> <C-o>j
imap <C-k> <C-o>k
imap <C-l> <C-o>l

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

nmap <silent> <special> <leader>n :NERDTreeToggle<return>

map <leader>f :Ack<space>
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Plugins
call vundle#begin()
Bundle 'gmarik/vundle'

Bundle 'Buffergator'
Bundle 'mileszs/ack.vim'
Bundle 'tomtom/tlib_vim'
Bundle 'ervandew/supertab'
Bundle 'bling/vim-airline'
Bundle 'tpope/vim-fugitive'
Bundle 'honza/vim-snippets'
Bundle 'garbas/vim-snipmate'
Bundle 'tristen/vim-sparkup'
Bundle 'scrooloose/syntastic'
Bundle 'scrooloose/nerdtree.git'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'scrooloose/nerdcommenter'
Bundle 'slim-template/vim-slim.git'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'altercation/vim-colors-solarized.git'
call vundle#end()

filetype off
filetype plugin indent on

syntax on
set background=dark
colorscheme solarized

