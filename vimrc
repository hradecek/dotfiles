"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" File: ~/.vimrc                                                              "
" Author: Ivo Hradek <ivohradek@gmail.com>                                    "
" Description: Vim configuration file                                         "
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible
set encoding=utf-8

let mapleader="\<Space>"

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

nnoremap <leader>/ :noh<cr>
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
nmap <silent> <special> <leader>rt :TagbarToggle<return>

map <leader>f :Ack<space>
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Plugins
call vundle#begin()
Plugin 'gmarik/vundle'

" Plugin 'Buffergator'
" Plugin 'mileszs/ack.vim'
" Plugin 'tomtom/tlib_vim'
Plugin 'ervandew/supertab'
Plugin 'bling/vim-airline'
" Plugin 'majutsushi/tagbar'
" Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
" Plugin 'honza/vim-snippets'
" Plugin 'garbas/vim-snipmate'
Plugin 'tristen/vim-sparkup'
 Plugin 'derekwyatt/vim-scala'
" Plugin 'scrooloose/syntastic'
Plugin 'airblade/vim-gitgutter'
" Plugin 'Valloric/YouCompleteMe'
Plugin 'scrooloose/nerdtree.git'
" Plugin 'Lokaltog/vim-easymotion'
Plugin 'scrooloose/nerdcommenter'
" Plugin 'slim-template/vim-slim.git'
" Plugin 'MarcWeber/vim-addon-mw-utils'
" Plugin 'terryma/vim-multiple-cursors'
Plugin 'bronson/vim-trailing-whitespace'
Plugin 'altercation/vim-colors-solarized.git'

call vundle#end()

filetype off
filetype plugin indent on

syntax on
set background=dark
colorscheme solarized

