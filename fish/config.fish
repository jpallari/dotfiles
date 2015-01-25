# aliases
alias jk 'tmux attach -d'
alias sudo 'sudo '
alias emacs 'emacs -nw'
alias httpserver 'python -m SimpleHTTPServer 8888'
if [ (uname) = 'Linux' ]
    alias pbcopy 'xsel --clipboard --input'
    alias pbpaste 'xsel --clipboard --output'
end

# env variables
set -x PAGER "less"

# local configurations
[ -e $HOME/.local.fish ]; and source $HOME/.local.fish

