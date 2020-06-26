if exists('did_load_filetypes')
  finish
endif

augroup filetypedetect
    au BufRead,BufNewFile *.txt setf text
    au BufRead,BufNewFile *mutt-* setf mail
    au BufRead,BufNewFile *.md setf markdown
    au BufRead,BufNewFile Jenkinsfile setf groovy
augroup END

