" General
syntax on
set nocompatible
set showcmd
set autowrite
set mouse=a
set backspace=indent,eol,start
set ai
set nobackup
set ruler
set hidden
set scrolloff=3
set wildmenu
set wildignore=*.so,*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*/.git*,*/.hg*,*/.svn*
set ofu=syntaxcomplete#Complete
set completeopt=menuone,longest,preview
set sw=4 sts=4 ts=8 et
set tw=0
set pastetoggle=<F4>
set listchars=tab:▸\ ,eol:¬,trail:⋅,extends:❯,precedes:❮
let &showbreak='↳'
set showtabline=0
set splitbelow
set splitright

" Search & replace stuff
set gdefault
set showmatch
set ignorecase
set infercase
set smartcase
set incsearch
set nohls

" Functions
fu! BufferInfo()
    let name = bufname('%')
    let name = empty(l:name) ? '[No Name]' : l:name
    let maininfo = bufnr('%') . ' ' . name
    let infos = join([
        \ empty(&ft) ? '-' : &ft,
        \ empty(&fenc) ? '-' : &fenc,
        \ empty(&ff) ? '-' : &ff,
        \ &modified ? 'modified' : 'unmodified'
        \ ], ', ')
    echo l:maininfo . ': ' . l:infos
endfunction

fu! CommentLines() range
    let commentsymbol = exists('b:commentsymbol') ? b:commentsymbol : '//'
    let beginsWithComment = getline(a:firstline) =~ ('^' . l:commentsymbol)
    for linenum in range(a:firstline, a:lastline)
        let line = getline(linenum)
        let replacement = l:beginsWithComment
            \ ? substitute(line, '^' . l:commentsymbol . '\s\?', '', '')
            \ : substitute(line, '^', l:commentsymbol . ' ', '')
        call setline(linenum, replacement)
    endfor
    call cursor(a:lastline + 1, 1)
endfunction

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

" Custom commands
command! -nargs=0 BufferInfo call BufferInfo()
command! -nargs=0 -range Comment <line1>,<line2>call CommentLines()
command! -nargs=0 Here lcd %:p:h

" Custom mappings
noremap k gk
noremap j gj
nnoremap x "_dl
nnoremap X "_dh
nnoremap Y y$
vnoremap < <gv
vnoremap > >gv
nnoremap <Leader>p :call BufferInfo()<cr>
vnoremap <Leader>c "+y
nnoremap <Leader>v "+p
nnoremap <silent> <Esc>; :call CommentLines()<cr>
vnoremap <silent> <Esc>; :call CommentLines()<cr>
inoremap <M-Backspace> <C-w>
inoremap <Esc><Backspace> <C-w>
cnoremap <M-Backspace> <C-w>
cnoremap <Esc><Backspace> <C-w>
nnoremap <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

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
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Additional configurations
if filereadable($HOME . "/.vim/extra.vim")
    source $HOME/.vim/extra.vim
endif

" Local conf
if filereadable($HOME . "/.vimrc.local")
    source $HOME/.vimrc.local
endif

