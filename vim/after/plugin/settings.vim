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

