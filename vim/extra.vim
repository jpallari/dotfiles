" Vundle start
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'moll/vim-bbye'
Plugin 'bitc/vim-hdevtools'
Plugin 'jpalardy/vim-slime'
Plugin 'vim-scripts/gnupg.vim'
Plugin 'endel/vim-github-colorscheme'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-notes'

call vundle#end()
filetype plugin on
" Vundle end

" CtrlP
let g:ctrlp_map = ''
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 0
let g:ctrlp_max_files = 4000
let g:ctrlp_max_depth = 10
let g:ctrlp_follow_symlinks = 0
let g:ctrlp_dotfiles = 0
let g:ctrlp_custom_ignore = {
    \ 'dir': '\.cache$'
    \ }
nnoremap <Leader>f :CtrlP<cr>
nnoremap <Leader>b :CtrlPBuffer<cr>
nnoremap <Leader>r :CtrlPMRUFiles<cr>
nnoremap <Leader><Leader> :CtrlPBuffer<cr>

" bbye
nnoremap <Leader>q :Bdelete<CR>

" vim-slime
let g:slime_target = "tmux"
