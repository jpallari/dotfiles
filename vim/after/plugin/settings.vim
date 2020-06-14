" FZF overrides
if exists(':Files')
    if executable('rg')
        let $FZF_DEFAULT_COMMAND="rg --files --hidden --glob '!.git/*'"
    endif
    nnoremap <Leader>; :Files<cr>
endif

if exists(':GFiles')
    nnoremap ; :GFiles --cached --others --exclude-standard<cr>
endif

if executable('rg') && exists(':Rg')
    nnoremap <Leader>/ :Rg<space>
endif

if exists(':Commands')
    nnoremap <Leader>: :Commands<cr>
endif

if exists(':Buffers')
    nnoremap <Leader>b :Buffers<cr>
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

" NERDTree
let NERDTreeHijackNetrw = 1
let NERDTreeMinimalUI = 1
au FileType nerdtree nnoremap <buffer> <Leader>q :NERDTreeClose<cr>

