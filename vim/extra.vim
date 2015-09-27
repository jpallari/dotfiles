" Plugins
call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'moll/vim-bbye'
Plug 'bitc/vim-hdevtools'
Plug 'jpalardy/vim-slime'
Plug 'vim-scripts/gnupg.vim'
Plug 'mileszs/ack.vim'
Plug 'vim-scripts/paredit.vim', { 'for': ['clojure', 'scheme'] }
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'derekwyatt/vim-scala'
call plug#end()

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
map x "_dl
map X "_dh
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste
nmap <M-p> <Plug>yankstack_substitute_older_paste
nmap <M-n> <Plug>yankstack_substitute_newer_paste
nmap <Esc>p <Plug>yankstack_substitute_older_paste
nmap <Esc>n <Plug>yankstack_substitute_newer_paste

