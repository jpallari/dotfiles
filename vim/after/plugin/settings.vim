" FZF overrides
if exists(':Files')
    nnoremap ; :Files<cr>
endif

if exists(':GFiles')
    nnoremap <Leader>; :GFiles<cr>
endif

if executable('rg') && exists(':Rg')
    nnoremap <Leader>/ :Rg<space>
endif

if exists(':Commands')
    nnoremap <Leader>: :Commands<cr>
endif

" Dracula theme
try
    let g:dracula_colorterm = 0
    colorscheme dracula
catch
endtry

" Lightline overrides
if exists('g:loaded_lightline')
    set noshowmode
endif

" Git gutter customizations
highlight GitGutterAdd    guifg=#009900 ctermfg=2
highlight GitGutterChange guifg=#bbbb00 ctermfg=3
highlight GitGutterDelete guifg=#ff2222 ctermfg=1

" Lightline
let g:lightline = {
            \ 'active': {
            \    'left': [ [ 'mode', 'paste' ],
            \              [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
            \ },
            \ 'component_function': {
            \   'gitbranch': 'FugitiveHead'
            \ },
            \ }

" NERDTree
let NERDTreeHijackNetrw = 1
let NERDTreeMinimalUI = 1
au FileType nerdtree nnoremap <buffer> <Leader>q :NERDTreeClose<cr>

