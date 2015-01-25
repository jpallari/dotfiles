set nocompatible

" Buffers and files
set hidden
set autowrite
set nobackup
set noswapfile

" Editing
set backspace=indent,eol,start
set autoindent
set sw=4 sts=4 ts=8 et tw=0
set nojoinspaces

" UI
syntax on
set scrolloff=3
set showcmd
set mouse=a
set showtabline=0
set ruler
set splitbelow
set splitright
set listchars=tab:▸\ ,eol:¬,trail:⋅,extends:❯,precedes:❮
let &showbreak='↳'
set wrap
set linebreak
set visualbell t_vb=
set noerrorbells
set shortmess+=I
set title

" Menu
set wildmenu
set wildmode=longest,full
set wildignore+=*.so,*.dll,*.o,*.a,*.obj,*.exe,*.pyc,*.class
set wildignore+=.git,.hg,.svn
set wildignore+=*.bak,*.swp,.DS_Store,*.tmp,*~

" Completion
set ofu=syntaxcomplete#Complete
set completeopt=menuone,longest,preview

" Search & replace stuff
set gdefault
set showmatch
set ignorecase
set infercase
set smartcase
set incsearch
set nohls

" File type detection
filetype on
filetype plugin on
filetype indent on

" Folding
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent

augroup filetypedetect
    au BufRead,BufNewFile *.txt setfiletype text
    au BufRead,BufNewFile *mutt-* setfiletype mail
    au BufRead,BufNewFile *.md setfiletype markdown
augroup END

" Functions
fu! BufferInfo()
    let name = expand('%:t')
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

" Commands
command! -nargs=0 BufferInfo call BufferInfo()
command! -nargs=0 -range Comment <line1>,<line2>call CommentLines()
command! -nargs=0 Here lcd %:p:h

" Preferred defaults
set pastetoggle=<F4>
noremap k gk
noremap j gj
nnoremap <space> za
nnoremap x "_dl
nnoremap X "_dh
nnoremap Y y$
vnoremap < <gv
vnoremap > >gv
inoremap <M-Backspace> <C-w>
inoremap <Esc><Backspace> <C-w>
cnoremap <M-Backspace> <C-w>
cnoremap <Esc><Backspace> <C-w>

" Custom command mappings
nnoremap <Leader>p :BufferInfo<cr>
nnoremap <silent> <Esc>; :Comment<cr>
vnoremap <silent> <Esc>; :Comment<cr>

" Autocompleted commands
nnoremap <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" Copy/paste
vnoremap <Leader>c "+y
nnoremap <Leader>v "+p

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
nnoremap <Leader>tb :set linebreak! linebreak?<cr>
nnoremap <Leader>tc :set cursorline! cursorline?<cr>

" Colors
hi statusline term=inverse,bold cterm=inverse,bold ctermfg=darkred ctermbg=white
hi statuslinenc term=inverse,bold cterm=inverse,bold ctermfg=gray ctermbg=black

" shell
if &shell =~# 'fish$'
    set shell=sh
endif

" Additional configurations
if filereadable($HOME . "/.vim/extra.vim")
    source $HOME/.vim/extra.vim
endif

" Local conf
if filereadable($HOME . "/.vimrc.local")
    source $HOME/.vimrc.local
endif

