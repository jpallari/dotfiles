" jkpl's vimrc
"

" Filetype detection
filetype on
filetype plugin on
filetype indent on
augroup filetypedetect
    au BufRead,BufNewFile *.txt setfiletype text
    au BufRead,BufNewFile *mutt-* setfiletype mail
    au BufRead,BufNewFile *.md setfiletype markdown
augroup END
au FileType markdown set tw=79 sw=4 sts=4 et
au FileType text set tw=79 sw=4 sts=4 et
au FileType mail set tw=65

" General
syntax on
set nocompatible
set background=dark
set showcmd
set showmatch
set ignorecase
set smartcase
set incsearch
set autowrite
set mouse=a
set backspace=indent,eol,start
set ai
set nobackup
set ruler 
set hidden
set wildmenu
set wildignore=*.o,*.obj,*.bak,*.exe
set hlsearch
set sw=4 sts=4 et
set ofu=syntaxcomplete#Complete
set completeopt=menuone,longest,preview
set tw=0
set pastetoggle=<F4>
let mapleader=","

" Statusline
set laststatus=2
set statusline=%t\ %y\ [%{strlen(&fenc)?&fenc:'none'},%{&ff}]\ %h\ %m\ %r\%=%c,%l\ %LL\ %P

" Syntastic
let g:syntastic_enable_highlighting = 0
let g:syntastic_mode_map = { 'mode': 'passive',
                           \ 'active_filetypes': [],
                           \ 'passive_filetypes': [] }

" Haskell
au BufEnter *.hs compiler ghc
let g:haddock_browser="/usr/bin/dwb"

" Some terminals just don't know how many colors they can actually display
if $COLORTERM == "Terminal"
    " for those newfangled vte terminals
    set t_Co=256
elseif $COLORTERM =~ "rxvt-xpm"
    " for rxvt-xpm based color terminals (eg. rxvt-unicode)
    set t_Co=256
else
    " incase everything else fails
    "set t_Co=16
endif

" Choose a theme based on the available colors
if &t_Co > 16
    colorscheme jellybeans
else
    colorscheme robokai
endif

" Jump back to where we left
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Arrowkey fix for rxvt-unicode
if &term == 'rxvt-unicode'
    imap OA <Up>
    imap OB <Down>
    imap OC <Right>
    imap OD <Left>
endif

" Keymaps
nnoremap <F2> :NERDTreeToggle<CR>
nnoremap <Leader>f :NERDTreeToggle<CR>
nnoremap <C-n> :bn<CR>
nnoremap <C-p> :bp<CR>
nnoremap Y y$
noremap <buffer> <silent> k gk
noremap <buffer> <silent> j gj
noremap x "_dl
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
inoremap <expr> <C-Space> pumvisible() \|\| &omnifunc == '' ?
            \ "\<lt>C-n>" :
            \ "\<lt>C-x>\<lt>C-o><c-r>=pumvisible() ?" .
            \ "\"\\<lt>c-n>\\<lt>c-p>\\<lt>c-n>\" :" .
            \ "\" \\<lt>bs>\\<lt>C-n>\"\<CR>"
imap <C-@> <C-Space>
nnoremap <silent> <Leader>/ :set hlsearch!<cr>
nnoremap <Leader>p :setlocal paste! paste?<cr>
noremap ; :
nnoremap - <C-w>-
nnoremap + <C-w>+

" Some Emacs keybindings
inoremap <C-p> <C-O>k
inoremap <C-n> <C-O>j
inoremap <C-f> <C-O>l
inoremap <C-b> <C-O>h
inoremap <C-e> <C-O>$
inoremap <C-a> <C-O>0
inoremap <C-k> <C-O>D
cnoremap <C-f> <Right>
cnoremap <C-b> <Left>

"
" Some functions...
"
function! SpaceIndent (...)
    if a:0 > 0
        let &l:shiftwidth = a:1
        let &l:softtabstop = a:1
        let len = a:1
    else
        setl sw=4 sts=4
        let len = 4
    endif
    setl et
    echo "Space indent set to " . len
endfunction

function! TabIndent (...)
    if a:0 > 0
        let &shiftwidth = a:1
        let &tabstop = a:1
        let len = a:1
    else
        setl sw=8 ts=8
        let len = 8
    endif
    setl noet sts=0
    echo "Tab indent set to " . len
endfunction

command! -nargs=1 SpaceIndent :call SpaceIndent(<q-args>)
command! -nargs=1 TabIndent :call TabIndent(<q-args>)

" Pathogen
call pathogen#infect()

" Source a local configuration file if available
if filereadable($HOME . "/.vimrc.local")
    source $HOME/.vimrc.local
endif

