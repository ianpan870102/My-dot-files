"Plugin (Using Tpope's pathogen)
execute pathogen#infect()
filetype plugin indent on
set nocompatible
set wrapmargin=8
set encoding=utf-8
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Autocommand to remove trailing whitespace on save
" winsaveview & winrestview make sure that the cursor
" position stays the same.
fun! Trimwhitespace()
    let l:save = winsaveview()
    %s/\s\+$//e
    call winrestview(l:save)
endfun
autocmd BufWritePre * call Trimwhitespace()
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set splitbelow
set splitright
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"General Settings

"Command autocomplete (wildmenu)
set wildmenu
set wildmode=longest:list,full

"Line numbers and other basic features
set numberwidth=2
set ruler
set lbr
set backspace=2

" Indentation
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set autoindent

" Colors
syntax on

" Show incomplete commands
set showcmd

" For searching
set hlsearch
set incsearch
set ignorecase
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set dictionary="/usr/dict/words"
