execute pathogen#infect()
filetype plugin indent on 
set nocompatible
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Indent Guides
"To toggle on/off: <leader>ig
"let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors = 0
""let g:indent_guides_start_level = 2
""let g:indent_guides_guide_size = 2
"autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd ctermbg=30
"autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=23
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd ctermbg=30
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=24
"~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
"Indent Lines
"Enabled by default
let g:indentLine_color_term = 242
"let g:indentLine_bgcolor_term = 202
let g:indentLine_char = '¦'
"let g:indentLine_char = '│'
"let g:indentLine_char = ' '
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"General Settings
"set cursorline
"hi CursorLine   cterm=bold ctermbg=darkgrey ctermfg=NONE
set number 
set relativenumber 
hi LineNr  cterm=NONE ctermbg=24 ctermfg=44
"hi LineNr  cterm=NONE ctermbg=236 ctermfg=246
set numberwidth=2
set ruler
set lbr
set mouse=
set splitbelow
set splitright
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Changing syntax colours of keywords (:highlight)
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
    "visually select word"
    nmap <space> l

    let mapleader = ";"
    "navigation in normal mode"
    nnoremap J <C-e>
    nnoremap K <C-y>
    nnoremap <leader>a 0
    nnoremap a<leader> $
    nnoremap d<leader>a d0
    nnoremap da<leader> d$
    nnoremap c<leader>a c0
    nnoremap ca<leader> c$
    nnoremap y<leader>a y0
    nnoremap ya<leader> y$

    nnoremap <cr> gg  
    nnoremap z zz  

    nnoremap <leader>f <C-f>
    nnoremap <leader>b <C-b>
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Self-written snippets
" C-snippets
inoremap #IO #include<space><stdio.h>
inoremap #LIB #include<space><stdlib.h>
inoremap MAIN int<space>main(void)  {<cr><cr><cr><cr>return 0;<cr><C-c>0i}<C-c>0kkkki<Tab>
inoremap FOI for(i = 0; i <; i++) {<cr><C-c>0i}<C-c>0kw14li<space>
inoremap FOJ for(j = 0; j <; j++) {<cr><C-c>0i}<C-c>0kw14li<space>
inoremap WHILE while()  {<cr>}<C-c>0kkw6li
inoremap PF printf("");<C-c>hhi
inoremap SF scanf("");<C-c>hhi
inoremap IF if()<C-c>A  {<cr>}<C-c>0kw3li
inoremap ELIF else if()<C-c>A  {<cr>}<C-c>0kw8li
inoremap FN function() 
" JavaScript / Node.js-snippets
inoremap C.L console.log();<C-c>hi
inoremap C>L console.log();<C-c>hi
inoremap M.E module.exports = ;<C-c>i
inoremap M>E module.exports = ;<C-c>i
inoremap RQ require();<C-c>hi
"HTML template
inoremap HTML <!DOCTYPE html><cr><cr><html lang="en"><cr><cr><BS><head><cr><meta charset="UTF-8"><cr><cr><title></title><cr></head><cr><cr><body><cr><cr></body><cr><cr></html><C-c>

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
autocmd Filetype html setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype css setlocal ts=2 sts=2 sw=2 expandtab
autocmd Filetype javascript setlocal ts=2 sts=2 sw=2 expandtab
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
"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set dictionary="/usr/dict/words"
inoremap ( ()<C-c>i
inoremap { {}<C-c>i
inoremap [ []<C-c>i
inoremap " ""<C-c>i
inoremap ' ''<C-c>i
imap <leader>i <cr><C-o>k<Tab>
