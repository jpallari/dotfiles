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
set showcmd
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
set wildmode=list:longest,full
set wildignore+=*.so,*.dll,*.o,*.a,*.obj,*.exe,*.pyc,*.class
set wildignore+=.git,.hg,.svn
set wildignore+=*.bak,*.swp,.DS_Store,*.tmp,*~

" Completion
set ofu=syntaxcomplete#Complete
set completeopt=menuone,longest,preview

" Search & replace
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
    let beginsWithComment = getline(a:firstline) =~ ('\M^' . l:commentsymbol)
    for linenum in range(a:firstline, a:lastline)
        let line = getline(linenum)
        let replacement = l:beginsWithComment
            \ ? substitute(line, '\M^' . l:commentsymbol . '\s\?', '', '')
            \ : l:commentsymbol . ' ' . line
        if exists('b:commentsymbolend')
            let l:replacement = l:beginsWithComment
                \ ? substitute(l:replacement, '\M\s\?' . b:commentsymbolend . '$', '', '')
                \ : l:replacement . ' ' . b:commentsymbolend
        endif
        call setline(linenum, replacement)
    endfor
    call cursor(a:lastline + 1, 1)
endfunction

fu! CommentSymbol(start, ...)
    let b:commentsymbol = a:start
    if a:0 >= 1
        let b:commentsymbolend = a:1
    elseif exists('b:commentsymbolend')
        unlet b:commentsymbolend
    endif
endfunction

fu! Indent(i)
    let n = str2nr(a:i)
    if a:i ==? 'tab' || a:i ==? 't'
        setl noet sts=0 sw=8
        echo 'Using tab indentation'
    elseif l:n > 0
        setl et
        let &sts=l:n
        let &sw=l:n
        echo 'Using indentation level ' . l:n
    else
        echoerr 'Invalid indentation parameter'
    endif
endfunction

function! s:RunShellCommand(cmdline)
  let expanded_cmdline = a:cmdline
  for part in split(a:cmdline, ' ')
     if part[0] =~ '\v[%#<]'
        let expanded_part = fnameescape(expand(part))
        let expanded_cmdline = substitute(expanded_cmdline, part, expanded_part, '')
     endif
  endfor
  botright new
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
  call setline(1, a:cmdline)
  call setline(2, substitute(a:cmdline, '.', '=', 'g'))
  execute '$read !'. expanded_cmdline
  setlocal nomodifiable
  1
endfunction

" Commands
command! -nargs=0 BufferInfo call BufferInfo()
command! -nargs=0 -range Comment <line1>,<line2>call CommentLines()
command! -nargs=+ CommentSymbol call CommentSymbol(<f-args>)
command! -nargs=0 Here lcd %:p:h
command! -nargs=1 Indent call Indent(<f-args>)
command! -complete=shellcmd -nargs=+ Shell call s:RunShellCommand(<q-args>)

" Preferred defaults
noremap k gk
noremap j gj
nnoremap <space> za
map x "_dl
map X "_dh
map Y y$
vnoremap < <gv
vnoremap > >gv
inoremap <M-Backspace> <C-w>
inoremap <Esc><Backspace> <C-w>
cnoremap <M-Backspace> <C-w>
cnoremap <Esc><Backspace> <C-w>

" Custom command mappings
nnoremap <Leader>i :BufferInfo<cr>
nnoremap <silent> <M-;> :Comment<cr>
nnoremap <silent> <Esc>; :Comment<cr>
vnoremap <silent> <M-;> :Comment<cr>
vnoremap <silent> <Esc>; :Comment<cr>

" Autocompleted commands
nnoremap <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" Copy/paste
vnoremap <Leader>c "+y
nnoremap <Leader>v "+p

" Ctrl+Space for omnicompletion
inoremap <expr> <C-Space> pumvisible() \|\| &omnifunc == '' ?
            \ "\<lt>C-n>" :
            \ "\<lt>C-x>\<lt>C-o><c-r>=pumvisible() ?" .
            \ "\"\\<lt>c-n>\\<lt>c-p>\\<lt>c-n>\" :" .
            \ "\" \\<lt>bs>\\<lt>C-n>\"\<CR>"
imap <C-@> <C-Space>

" Toggles
nnoremap <Leader>th :set hlsearch! hlsearch?<cr>
nnoremap <Leader>tp :setl paste! paste?<cr>
nnoremap <Leader>tn :set number!<cr>
nnoremap <Leader>tl :set list! list?<cr>
nnoremap <Leader>tw :set wrap! wrap?<cr>
nnoremap <Leader>tc :set cursorline! cursorline?<cr>
nnoremap <Leader>ts :setl spell! spell?<cr>

" Replace commands
nnoremap <Leader>rw :%s/\s\+$//e<cr>

" Mouse
set mouse=a

" Colors
hi statusline term=inverse,bold cterm=inverse,bold ctermfg=darkred ctermbg=white
hi statuslinenc term=inverse,bold cterm=inverse,bold ctermfg=gray ctermbg=black

" Additional configurations
if filereadable($HOME . "/.vim/extra.vim")
    source $HOME/.vim/extra.vim
endif

" Local conf
if filereadable($HOME . "/.vimrc.local")
    source $HOME/.vimrc.local
endif

