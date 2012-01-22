set nocompatible
set background=dark
syntax on
set showcmd             " Show (partial) command in status line.
set showmatch           " Show matching brackets.
set ignorecase          " Do case insensitive matching
set smartcase           " Do smart case matching
set incsearch           " Incremental search
set autowrite           " Automatically save before commands like :next and :make
set mouse=a             " Enable mouse usage (all modes)
set backspace=indent,eol,start " Backspace behaviour
set ai       " Autoindent
set number   " Line numbers
set nobackup " No backup files
set ruler 
set hidden
set wildmenu
set sw=4 sts=4 et " Default tab behaviour
set ofu=syntaxcomplete#Complete " OmniCompletion stuff
set completeopt=menuone,longest,preview
set tw=0
set pastetoggle=<F4>

" Filetype detection
filetype on
filetype plugin on
filetype plugin indent on
au FileType markdown set tw=79 sw=4 sts=4
au FileType mail set tw=65

" Statusline
set laststatus=2
set statusline=%t\ %y\ [%{strlen(&fenc)?&fenc:'none'},%{&ff}]\ %h\ %m\ %r\%=%c,%l\ %LL\ %P
" Powerline
let g:Powerline_cache_file = $HOME . "/.vim/Powerline.cache"
let g:Powerline_theme = "jj"

" Supertab
let g:SuperTabDefaultCompletionType = "context"

" Syntastic
let g:syntastic_enable_highlighting = 0
let g:syntastic_mode_map = { 'mode': 'passive',
                           \ 'active_filetypes': [],
                           \ 'passive_filetypes': [] }

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
    colorscheme molokai
else
    colorscheme robokai
endif

" Jump back to where we left
"if has("autocmd")
"  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
"endif

" Arrowkey fix for rxvt-unicode
if &term == 'rxvt-unicode'
    imap OA <Up>
    imap OB <Down>
    imap OC <Right>
    imap OD <Left>
endif

" Keymaps
nnoremap <F2> :NERDTreeToggle<CR>
nnoremap <C-n> :bn<CR>
nnoremap <C-p> :bp<CR>
nnoremap Y y$
noremap <buffer> <silent> k gk
noremap <buffer> <silent> j gj

augroup filetypedetect
    " Mail
    autocmd BufRead,BufNewFile *mutt-* setfiletype mail

    " Markdown instead of modula
    autocmd BufRead,BufNewFile *.md set filetype=markdown
augroup END

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

