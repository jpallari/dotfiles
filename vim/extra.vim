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
