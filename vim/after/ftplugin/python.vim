set omnifunc=pythoncomplete#Complete
set sw=4 sts=4 et sta tw=0 tw=79
set foldmethod=indent
set foldlevel=99
highlight WhitespaceEOL ctermbg=darkgray guibg=darkgray
match WhitespaceEOL /\s\+$/
python << EOF
import os
import sys
import vim
if 'VIRTUAL_ENV' in os.environ:
    project_base_dir = os.environ['VIRTUAL_ENV']
    sys.path.insert(0, project_base_dir)
    activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
    execfile(activate_this, dict(__file__=activate_this))
EOF
