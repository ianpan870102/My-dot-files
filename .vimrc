call plug#begin('~/.vim/plugged')
Plug 'romgrk/doom-one.vim'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-commentary'
call plug#end()

filetype plugin indent on
syntax on
colo doom-one
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
set mouse=a
set mouseshape=v:arrow
set laststatus=2

" Package config
let g:NERDTreeShowHidden=1
autocmd FileType nerdtree nmap <Tab> <CR>
nnoremap <silent> <C-S-e> :NERDTreeToggle<CR>
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
    \ quit | endif

" GUI config
set guifont=Hack\ 15
set guioptions=i
set lines=35
set columns=65
