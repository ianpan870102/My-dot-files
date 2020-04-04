" Vim plug
call plug#begin('~/.vim/plugged')
Plug 'tomasiser/vim-code-dark'
Plug 'tpope/vim-commentary'
Plug 'jiangmiao/auto-pairs'
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
autocmd FileType python setlocal sw=2 sts=2 et
