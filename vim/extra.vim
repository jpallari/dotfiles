" Vundle start
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'moll/vim-bbye'
Plugin 'bitc/vim-hdevtools'
Plugin 'jpalardy/vim-slime'
Plugin 'vim-scripts/gnupg.vim'
Plugin 'xolox/vim-misc'
Plugin 'szw/vim-ctrlspace'
Plugin 'mileszs/ack.vim'
Plugin 'vim-scripts/paredit.vim'
Plugin 'fatih/vim-go'
Plugin 'dag/vim-fish'
Plugin 'maxbrunsfeld/vim-yankstack'

call vundle#end()
filetype plugin on
" Vundle end

" bbye
nnoremap <Leader>q :Bdelete<CR>

" vim-slime
let g:slime_target = "tmux"
let g:slime_no_mappings = 1
xmap <Leader>s <Plug>SlimeRegionSend
nmap <Leader>s <Plug>SlimeParagraphSend

" Yankstack
let g:yankstack_map_keys = 0
call yankstack#setup()
map Y y$
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste
nmap <M-p> <Plug>yankstack_substitute_older_paste
nmap <M-n> <Plug>yankstack_substitute_newer_paste
nmap <Esc>p <Plug>yankstack_substitute_older_paste
nmap <Esc>n <Plug>yankstack_substitute_newer_paste
