"Plugin (Using Tpope's pathogen)
execute pathogen#infect()
filetype plugin indent on
set nocompatible

"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Autocommand to remove trailing whitespace on save"
autocmd BufWritePre * %s/\s\+$//e
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"NerdTree
nnoremap <C-n> :NERDTreeToggle<CR>
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Split windows
nnoremap <space>w/ :vsp<cr>
nnoremap <space>w- :sp<cr>
nnoremap <space>wj <C-W><C-J>
nnoremap <space>wk <C-W><C-K>
nnoremap <space>wl <C-W><C-L>
nnoremap <space>wh <C-W><C-H>
nnoremap <space>wd :hide<cr>
set splitbelow
set splitright
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" EasyMotion
nmap f <Plug>(easymotion-s)
let g:EasyMotion_smartcase = 1
imap <ESC> <C-c>
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Indent Lines
"Enabled by default
let g:indentLine_color_term = 242
"let g:indentLine_bgcolor_term = 202
let g:indentLine_char = '¦'
"let g:indentLine_char = '│'
"let g:indentLine_char = ' '
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"General Settings

"Command autocomplete (wildmenu)
set wildmenu
set wildmode=longest:list,full

"Toggle Relative Number (Ctrl + j)
nnoremap <C-J> :set<space>nu!<cr>:set<space>relativenumber!<cr>

"Line numbers and other basic features
hi LineNr  cterm=NONE ctermbg=NONE ctermfg=249
set numberwidth=2
set ruler
set lbr
set mouse=
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"(Personal preference)Changing syntax colours of keywords (:highlight)
hi cComment  cterm=italic ctermfg=37
hi vimComment  cterm=italic ctermfg=37
hi cType  cterm=bold,italic ctermfg=80
hi pythonComment  cterm=italic ctermfg=37
hi Structure  cterm=bold ctermfg=80
hi StorageClass  cterm=bold,italic ctermfg=80
hi Number cterm=bold ctermfg=154
hi Tag            cterm=bold ctermfg=197
hi SpecialChar    cterm=bold ctermfg=197
hi Delimiter      cterm=bold ctermfg=197
hi SpecialComment cterm=bold ctermfg=197
hi Debug          cterm=bold ctermfg=197
hi pythonEscape          cterm=bold ctermfg=197
hi pythonDoctest          cterm=bold ctermfg=197
hi pythonAttribute          cterm=bold,italic ctermfg=197
hi pythonSync          cterm=bold ctermfg=197
hi javaScriptPropietaryMethods  cterm=bold ctermfg=197
hi javaScriptEventListenerMethods  cterm=bold ctermfg=197
hi javaScriptParen  cterm=bold ctermfg=197
hi javaScriptDotNotation  cterm=bold ctermfg=197
hi javaScriptFunctionKey  cterm=bold ctermfg=197
hi javaScriptObjectKey  cterm=bold ctermfg=197
hi javaScriptTemplateVar  cterm=bold ctermfg=197
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Mapping keys
nmap <space> l

let mapleader = ";"
"navigation in normal mode"
nnoremap <leader>a 0
nnoremap a<leader> $
nnoremap d< eader>a d0
nnoremap da<leader> d$
nnoremap c<leader>a c0
nnoremap ca<leader> c$
nnoremap y<leader>a y0
nnoremap ya<leader> y$
nnoremap <space>fs :w<cr>

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
set backspace=indent,eol
augroup specific_filetype_indentations
    autocmd!
    autocmd Filetype html setlocal ts=2 sts=2 sw=2 expandtab
    autocmd Filetype css setlocal ts=2 sts=2 sw=2 expandtab
    autocmd Filetype javascript setlocal ts=2 sts=2 sw=2 expandtab
augroup END

imap <leader>i <cr><C-o>k<Tab>

"Autoindent everything correctly while startup
augroup autoindent_the_whole_file
    autocmd!
    autocmd VimEnter * :normal migg=G`i
augroup END
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Colorscheme
syntax on
hi Visual cterm=NONE ctermbg=244 ctermfg=226
hi Pmenu cterm=italic ctermfg=white ctermbg=19
hi PmenuSel cterm=italic ctermfg=white ctermbg=33
hi MatchParen cterm=bold ctermbg=243 ctermfg=white
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" Show incomplete commands
set showcmd
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" For searching
set hlsearch
set incsearch
set ignorecase
hi Search cterm=bold ctermbg=63 ctermfg=white
nnoremap <space><space> :noh<cr>
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set dictionary="/usr/dict/words"
inoremap ( ()<C-c>i
inoremap { {}<C-c>i
inoremap [ []<C-c>i
inoremap " ""<C-c>i
inoremap ' ''<C-c>i
