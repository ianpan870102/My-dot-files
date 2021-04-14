" Vim plug
call plug#begin('~/.vim/plugged')
Plug 'tomasiser/vim-code-dark'
Plug 'joshdick/onedark.vim'
Plug 'morhetz/gruvbox'

Plug 'tpope/vim-commentary'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'scrooloose/nerdtree'
Plug 'airblade/vim-gitgutter'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'sheerun/vim-polyglot'
" Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
" Plug 'junegune/fzf.vim' " TODO: can't find repo (?)
" Plug 'nvim-treesitter/nvim-treesitter' " TODO: neovim nightly build fails (issue opened)
call plug#end()

filetype plugin indent on
syntax on
set termguicolors
colo onedark " codedark, onedark, gruvbox
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
set ignorecase
set smartcase
autocmd FileType python setlocal sw=2 sts=2 et
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

set path+=**
set wildmenu
set wildignore+=**/node_modules/**
set hidden
set mouse=a
set encoding=utf-8
set nobackup
set nowritebackup
" set cmdheight=2
set updatetime=300
set timeoutlen=400
set shortmess+=c

imap <C-BS> <C-W>

" Package configurations
let g:NERDTreeShowHidden=1
nnoremap <silent><C-S-e> :NERDTreeToggleVCS<CR>
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
    \ quit | endif

let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
let g:ctrlp_root_markers = ['.projectile']
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
let g:ctrlp_custom_ignore = '[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '[\/]\.(git|hg|svn)$',
  \ 'file': '\.(exe|so|dll)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }
let g:ctrlp_user_command = 'rg --vimgrep %s'
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

let g:coc_global_extensions = [
  \ 'coc-clangd',
  \ 'coc-tsserver',
  \ 'coc-pyright',
  \ 'coc-pairs',
  \ ]

if has("patch-8.1.1564")
  set signcolumn=number
else
  set signcolumn=yes
endif

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gD <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Formatting selected code with Alt-Shift-f
" nnoremap <A-S-f>  <Plug>(coc-format-selected)
" nnoremap <M-S-f>  <Plug>(coc-format-selected)
nnoremap <A-S-f> :Format<cr>
nnoremap <M-S-f> :Format<cr>

" Neovide GUI configurations
let g:neovide_cursor_animation_length=0.03
let g:neovide_cursor_trail_length=0.01
let g:neovide_cursor_antialiasing=v:true
set clipboard=unnamedplus
set guifont=Consolas:h13

let s:clip = '/mnt/c/Windows/System32/clip.exe' 
if executable(s:clip)
  augroup WSLYank
    autocmd!
    autocmd TextYankPost * call system('echo '.shellescape(join(v:event.regcontents, "\<CR>")).' | '.s:clip)
  augroup END
end

map <silent> "=p :r !powershell.exe -Command Get-Clipboard<CR>
map! <silent> <C-r>= :r !powershell.exe -Command Get-Clipboard<CR>

" noremap "*p :exe 'norm a'.system('/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -Command Get-Clipboard')<CR>
