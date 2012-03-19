" jkpl's vimrc
"

syntax on
set nocompatible
set background=dark
set showcmd
set gdefault
set autowrite
set mouse=a
set backspace=indent,eol,start
set ai
set nobackup
set ruler
set hidden
set scrolloff=3
set wildmenu
set wildignore=*.o,*.obj,*.bak,*.exe
set ofu=syntaxcomplete#Complete
set completeopt=menuone,longest,preview
set sw=4 sts=4 ts=8 et
set tw=0
set pastetoggle=<F4>
set listchars=tab:▸\ ,eol:¬,trail:⋅,extends:❯,precedes:❮
let mapleader=","
let &showbreak='↳'

" Search stuff
set showmatch
set ignorecase
set smartcase
set incsearch
set nohls

" Statusline
set laststatus=2
set statusline=\ %<%.40f
set statusline+=\ %y\ %m\ %r
set statusline+=%=
set statusline+=%c,%l\ %LL\ %P

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

" Syntastic
let g:syntastic_enable_highlighting = 0
let g:syntastic_mode_map = { 'mode': 'passive',
                           \ 'active_filetypes': [],
                           \ 'passive_filetypes': [] }

" Buftabs
let g:buftabs_only_basename=1

" Haskell
au BufEnter *.hs compiler ghc
let g:haddock_browser="/usr/bin/firefox"

" Some terminals just don't know how many colors they can actually display
if $COLORTERM =~ "rxvt-xpm"
    set t_Co=256
endif

" Choose a theme based on the available colors
if &t_Co > 88
    colorscheme jellybeans
else
    colorscheme lucius
endif

" Jump back to where we left
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Custom mappings
"

noremap ; :
noremap k gk
noremap j gj
noremap / /\v
noremap <F1> <ESC>
inoremap <F1> <ESC>
nnoremap x "_dl
nnoremap X "_dh
nnoremap Y y$
nnoremap <C-n> :bn<CR>
nnoremap <C-p> :bp<CR>
nnoremap <Leader>p :echo &ft . ", " . &fenc . ", " . &ff<cr>
nnoremap <Leader>f :LustyFilesystemExplorer<cr>
nnoremap <Leader>b :LustyBufferExplorer<cr>
nnoremap <Leader><Leader> <C-^>
vnoremap < <gv
vnoremap > >gv

" Indent stuff
nnoremap <Leader>ms :setl et sts=2 sw=2 ts=8 sts? sw? ts?<cr>
nnoremap <Leader>mm :setl et sts=4 sw=4 ts=8 sts? sw? ts?<cr>
nnoremap <Leader>ml :setl et sts=8 sw=8 ts=8 sts? sw? ts?<cr>
nnoremap <Leader>mt :setl noet sts=0 sw=8 ts=8 sts? sw? ts?<cr>

" Ctrl+Space for omnicompletion
inoremap <expr> <C-Space> pumvisible() \|\| &omnifunc == '' ?
            \ "\<lt>C-n>" :
            \ "\<lt>C-x>\<lt>C-o><c-r>=pumvisible() ?" .
            \ "\"\\<lt>c-n>\\<lt>c-p>\\<lt>c-n>\" :" .
            \ "\" \\<lt>bs>\\<lt>C-n>\"\<CR>"
imap <C-@> <C-Space>

" Toggle stuff
nnoremap <silent> <Leader>/ :set hlsearch!<cr>
nnoremap <Leader>tp :setlocal paste! paste?<cr>
nnoremap <Leader>tn :set number!<cr>
nnoremap <Leader>tr :set relativenumber!<cr>
nnoremap <Leader>tl :set list! list?<cr>
nnoremap <Leader>tw :set wrap! wrap?<cr>

" Shortcuts for managing windows
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
nnoremap - <C-w>-
nnoremap + <C-w>+

" Shortcuts for managing splits
nnoremap <Leader>swh :topleft vnew<cr>
nnoremap <Leader>swj :botright new<cr>
nnoremap <Leader>swk :topleft new<cr>
nnoremap <Leader>swl :botright vnew<cr>
nnoremap <Leader>sh :leftabove vnew<cr>
nnoremap <Leader>sj :rightbelow new<cr>
nnoremap <Leader>sk :leftabove new<cr>
nnoremap <Leader>sl :rightbelow vnew<cr>

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
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" Pathogen
call pathogen#infect()

" Source a local configuration file if available
if filereadable($HOME . "/.vimrc.local")
    source $HOME/.vimrc.local
endif

