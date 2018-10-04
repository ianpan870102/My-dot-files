"Plugin (Using Tpope's pathogen)
execute pathogen#infect()
filetype plugin indent on
set nocompatible

set wrapmargin=8

set encoding=utf-8

let g:auto_save = 1

let g:airline_powerline_fonts=1

hi VertSplit cterm=NONE
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"To prevent markdown syntax from concealing symbols
" (However this autocmd will hide indent guide lines as well)
" autocmd VimEnter * set conceallevel=0
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
"NERDTree
nnoremap <silent> <C-n> :NERDTreeToggle<CR>
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Split windows
nnoremap <silent> <space>w/ :vsp<cr>
nnoremap <silent> <space>w- :sp<cr>
nnoremap <space>wh <C-W><C-H>
nnoremap <space>wj <C-W><C-J>
nnoremap <space>wk <C-W><C-K>
nnoremap <space>wl <C-W><C-L>
nnoremap <silent> <space>wd :hide<cr>
set splitbelow
set splitright
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" EasyMotion
nmap f <Plug>(easymotion-s)
let g:EasyMotion_keys='ghasdflkjweroiu'
let g:EasyMotion_smartcase = 1
imap <ESC> <C-c>
hi EasyMotionTarget2First cterm=bold ctermfg=208
hi EasyMotionTarget2Second cterm=bold ctermfg=208
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Indent Lines  (Enabled by default)
"conceallevel has to be set to 1 or 2 for the characters to show!
let g:indentLine_color_term = 232
let g:indentLine_char = '┊'
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Code Length Limit Guideline
set colorcolumn=80
hi ColorColumn ctermbg=1
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"General Settings

"Command autocomplete (wildmenu)
set wildmenu
set wildmode=longest:list,full

"Toggle Relative Number (Ctrl + j)
nnoremap <silent> <C-J> :set<space>nu!<cr>:set<space>relativenumber!<cr>

"Line numbers and other basic features
hi LineNr  cterm=NONE ctermbg=NONE ctermfg=247
set nu
set relativenumber
set numberwidth=2
set ruler
set lbr
set mouse=
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Mapping keys
nmap <space> l

let mapleader = ";"
let maplocalleader = ";"
nnoremap <silent> <space>fs :w<cr>

nnoremap <cr> k
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Self-written snippets
augroup snippets_I_created
    autocmd!
    " C-snippets
    autocmd FileType c inoremap #IO #include<space><stdio.h>
    autocmd FileType c inoremap #LIB #include<space><stdlib.h>
    autocmd FileType c inoremap MAIN int<space>main(void)  {<cr><cr><cr><cr>return 0;<cr><C-c>0i}<C-c>0kkkki<Tab>
    autocmd FileType c inoremap FOI for(i = 0; i <; i++) {<cr><C-c>0i}<C-c>0kw14li<space>
    autocmd FileType c inoremap FOJ for(j = 0; j <; j++) {<cr><C-c>0i}<C-c>0kw14li<space>
    autocmd FileType c inoremap WHILE while()  {<cr>}<C-c>0kkw6li
    autocmd FileType c inoremap PF printf("");<C-c>hhi
    autocmd FileType c inoremap SF scanf("");<C-c>hhi
    " JavaScript / Node.js-snippets
    autocmd FileType javascript inoremap C.L console.log();<C-c>hi
    autocmd FileType javascript inoremap C>L console.log();<C-c>hi
    autocmd FileType javascript inoremap M.E module.exports = ;<C-c>i
    autocmd FileType javascript inoremap M>E module.exports = ;<C-c>i
    autocmd FileType javascript inoremap RQ require();<C-c>hi
    autocmd FileType javascript inoremap FN function()
    "HTML template
    autocmd FileType html inoremap HTML <!DOCTYPE html><cr><cr><html lang="en"><cr><cr><BS><head><cr><meta charset="UTF-8"><cr><cr><title></title><cr></head><cr><cr><body><cr><cr></body><cr><cr></html><C-c>
augroup END

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Indentation
set autoindent
set smartindent
set cindent
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set smarttab
set backspace=2
augroup specific_filetype_indentations
    autocmd!
    autocmd Filetype html setlocal ts=2 sts=2 sw=2 expandtab
    autocmd Filetype css setlocal ts=2 sts=2 sw=2 expandtab
    autocmd Filetype javascript setlocal ts=2 sts=2 sw=2 expandtab
augroup END

imap <leader>i <cr><C-o>k<Tab>

fun! Autoindenting()
    let l:save = winsaveview()
    :norm ggVG=
    call winrestview(l:save)
endfun
nnoremap <leader>i :call Autoindenting()<cr>

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Colorscheme
syntax on
colorscheme base16-gruvbox-dark-hard
let g:airline_theme='powerlineish'
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Show incomplete commands
set showcmd
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" For searching
set hlsearch
set incsearch
set ignorecase
hi Search cterm=NONE ctermbg=63 ctermfg=white
nnoremap <silent> <space><space> :noh<cr>
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set dictionary="/usr/dict/words"
