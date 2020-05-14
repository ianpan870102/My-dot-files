" Vim plug
call plug#begin('~/.vim/plugged')
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-commentary'
Plug 'tomasiser/vim-code-dark'
call plug#end()

filetype plugin indent on
syntax on
colo codedark
set nu
set et
set shiftwidth=2
set sts=2
set ai
set backspace=2
set hls
set is
set ru
set ttm=10
set clipboard=unnamed
autocmd FileType python setlocal sw=2 sts=2 et
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

set path+=**
set wildmenu
set wildignore+=**/node_modules/**
set hidden
