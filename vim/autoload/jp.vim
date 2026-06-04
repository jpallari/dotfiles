vim9script

const SID = expand('<SID>')

# ---------------------------------------------------------------------------
# Notification helpers
# ---------------------------------------------------------------------------

def NotifyInfo(msg: string)
  redraw
  echohl ModeMsg
  echom msg
  echohl None
enddef

def NotifyWarn(msg: string)
  redraw
  echohl WarningMsg
  echom msg
  echohl None
enddef

def NotifyErr(msg: string)
  redraw
  echohl ErrorMsg
  echom msg
  echohl None
enddef

# ---------------------------------------------------------------------------
# Indentation
# ---------------------------------------------------------------------------

export def SetIndent(level_str: string, mode: string)
  var lvl = level_str ==# '' ? 4 : str2nr(level_str)
  if lvl <= 0
    lvl = 4
  endif

  if mode ==# 'tab'
    setlocal noexpandtab
    setlocal softtabstop=0
  else
    setlocal expandtab
    &l:softtabstop = lvl
  endif

  &l:tabstop = lvl
  &l:shiftwidth = lvl
enddef

# ---------------------------------------------------------------------------
# File finding
# ---------------------------------------------------------------------------

export def FindFiles(pattern: string, cmd_name: string = '')
  if pattern ==# ''
    NotifyErr('Missing pattern')
    return
  endif

  const rg_cmd = ['rg', '--files', '--hidden',
                  '--color=never', '--smart-case',
                  '--iglob', '**/' .. pattern .. '*',
                  '--iglob', '!**/.git/*', '--']
  const find_cmd = ['find', '.', '-type', 'f', '-ipath', pattern]
  const git_cmd = ['git', 'ls-files', '--', pattern]
  var cmd: list<string> = []

  if cmd_name ==# 'rg'
    if executable('rg') == 1
      cmd = rg_cmd
    else
      NotifyErr('Command "rg" not found')
      return
    endif
  elseif cmd_name ==# 'find'
    cmd = find_cmd
  elseif cmd_name ==# 'git'
    cmd = git_cmd
  elseif cmd_name ==# ''
    cmd = executable('rg') == 1 ? rg_cmd : find_cmd
  else
    NotifyErr('Unknown command: ' .. cmd_name)
    return
  endif

  const cmd_str = join(mapnew(cmd, (_, v) => shellescape(v)), ' ')
  const lines = systemlist(cmd_str)
  if v:shell_error != 0
    NotifyErr('Find failed: ' .. join(lines, "\n"))
    return
  endif

  var list_contents: list<dict<any>> = []
  for line in lines
    if line !=# ''
      add(list_contents,
          {filename: line, lnum: 1, col: 1, text: ''})
    endif
  endfor

  setloclist(0, list_contents, 'r')
  setloclist(0, [], 'a', {title: 'Find: ' .. pattern})
  if !empty(list_contents)
    lfirst
  endif
enddef

# ---------------------------------------------------------------------------
# Visual / cursor selection
# ---------------------------------------------------------------------------

export def SelectionForCmdOpts(
    range_count: number, line1: number, line2: number
    ): list<list<number>>
  var start_row = 0
  var start_col = 0
  var end_row = 0
  var end_col = 0

  if range_count > 0
    const vstart = getpos("'<")
    const vend = getpos("'>")
    if vstart[1] > vend[1]
        || (vstart[1] == vend[1] && vstart[2] > vend[2])
      start_row = vend[1]
      start_col = vend[2]
      end_row = vstart[1]
      end_col = vstart[2]
    else
      start_row = vstart[1]
      start_col = vstart[2]
      end_row = vend[1]
      end_col = vend[2]
    endif
  else
    const pos = getpos('.')
    start_row = pos[1]
    start_col = pos[2]
    end_row = pos[1]
    end_col = pos[2]
  endif

  return [[start_row, start_col - 1], [end_row, end_col - 1]]
enddef

# ---------------------------------------------------------------------------
# Surround helpers
# ---------------------------------------------------------------------------

def AllSame(s: string): bool
  if s ==# '' || strlen(s) == 1
    return true
  endif
  const ch = strpart(s, 0, 1)
  var i = 1
  while i < strlen(s)
    if strpart(s, i, 1) !=# ch
      return false
    endif
    i += 1
  endwhile
  return true
enddef

export def MatchingPair(s: string): string
  if !AllSame(s)
    return s
  endif
  const ch = strpart(s, 0, 1)
  if ch ==# '('
    return repeat(')', strlen(s))
  elseif ch ==# '['
    return repeat(']', strlen(s))
  elseif ch ==# '{'
    return repeat('}', strlen(s))
  elseif ch ==# '<'
    return repeat('>', strlen(s))
  endif
  return s
enddef

def PosIsBefore(a: list<number>, b: list<number>): bool
  return a[0] < b[0] || (a[0] == b[0] && a[1] < b[1])
enddef

def PosIsEmpty(pos: list<number>): bool
  return empty(pos) || pos[0] == 0
enddef

def BufPutText(pos: list<number>, text: string)
  const lnum = pos[0]
  const col_ = pos[1]
  const line = getline(lnum)
  setline(lnum,
          strpart(line, 0, col_ - 1) .. text
            .. strpart(line, col_ - 1))
enddef

def BufSetText(pos: list<number>, text: string)
  const lnum = pos[0]
  const col_ = pos[1]
  const line = getline(lnum)
  setline(lnum,
          strpart(line, 0, col_ - 1) .. text
            .. strpart(line, col_ - 1 + strlen(text)))
enddef

def BufDelText(pos: list<number>, plen: number)
  const lnum = pos[0]
  const col_ = pos[1]
  const line = getline(lnum)
  setline(lnum,
          strpart(line, 0, col_ - 1)
            .. strpart(line, col_ - 1 + plen))
enddef

def TextPadLeft(a: string, b: string): string
  const trimmed = strpart(a, 0, strlen(b))
  return trimmed .. repeat(' ', max([0, strlen(b) - strlen(trimmed)]))
enddef

def TextPadRight(a: string, b: string): string
  const trimmed = strpart(a, 0, strlen(b))
  return repeat(' ', max([0, strlen(b) - strlen(trimmed)])) .. trimmed
enddef

export def FindPos(
    start_pos: list<number>, target: string, skip: string, left: bool
): list<number>
  const search_flags = left ? 'bnW' : 'nW'
  const target_pat = '\V' .. substitute(target, '\\', '\\\\', 'g')
  const skip_pat = '\V' .. substitute(skip, '\\', '\\\\', 'g')

  if target ==# skip
    const direct_pos = searchpos(target_pat, search_flags)
    if direct_pos[0] == 0
      return []
    endif
    return direct_pos
  endif

  # Work in 1-indexed col internally.
  var cur_pos = [start_pos[0], start_pos[1] + 1]
  var result: list<number> = []
  var between_count = 0

  while empty(result)
    cursor(cur_pos[0], cur_pos[1])
    const target_pos = searchpos(target_pat, search_flags)
    if target_pos[0] == 0
      break
    endif

    var skip_pos = searchpos(skip_pat, search_flags)
    if skip_pos[0] == 0
      result = target_pos
      break
    endif

    if (left && PosIsBefore(skip_pos, target_pos))
        || (!left && PosIsBefore(target_pos, skip_pos))
      if between_count > 0
        between_count -= 1
      else
        result = target_pos
        break
      endif
    endif

    var next_skip_pos = skip_pos
    while true
      cursor(next_skip_pos[0], next_skip_pos[1])
      next_skip_pos = searchpos(skip_pat, search_flags)
      if next_skip_pos[0] == 0
          || (left && PosIsBefore(next_skip_pos, target_pos))
          || (!left && PosIsBefore(target_pos, next_skip_pos))
        break
      endif
      between_count += 1
    endwhile

    cur_pos = target_pos
  endwhile

  cursor(start_pos[0], start_pos[1] + 1)
  return result
enddef

export def SurroundReplace(
    start_pos: list<number>, end_pos: list<number>,
    from_left: string, to_left_in: string
)
  if from_left ==# '' || to_left_in ==# ''
    NotifyWarn('Insufficient arguments for surround replace')
    return
  endif

  const from_right = MatchingPair(from_left)
  var to_right = MatchingPair(to_left_in)

  const to_left = TextPadLeft(to_left_in, from_left)
  to_right = TextPadRight(to_right, from_right)

  const left_pos = FindPos(start_pos, from_left, from_right, true)
  const right_pos = FindPos(end_pos, from_right, from_left, false)
  if PosIsEmpty(left_pos) || PosIsEmpty(right_pos)
    NotifyInfo('Nothing to replace')
    return
  endif

  BufSetText(right_pos, to_right)
  BufSetText(left_pos, to_left)
enddef

export def SurroundAdd(
    start_pos: list<number>, end_pos: list<number>,
    add_left: string, before_left_in: string = ''
)
  if add_left ==# ''
    NotifyWarn('Insufficient arguments for surround add')
    return
  endif

  const add_right = MatchingPair(add_left)
  var left_pos = copy(start_pos)
  var right_pos = copy(end_pos)

  left_pos[1] += 1
  right_pos[1] += 2

  if before_left_in !=# ''
    const before_left = TextPadLeft(before_left_in, add_left)
    const before_right = TextPadRight(
            MatchingPair(before_left_in), add_right)
    left_pos = FindPos(start_pos, before_left, before_right, true)
    right_pos = FindPos(end_pos, before_right, before_left, false)
    if PosIsEmpty(left_pos) || PosIsEmpty(right_pos)
      NotifyInfo('Nothing to replace')
      return
    endif
    left_pos[1] += 1
  endif

  BufPutText(right_pos, add_right)
  BufPutText(left_pos, add_left)
  # start_pos col is 0-indexed; cursor() takes 1-indexed col.
  cursor(start_pos[0], start_pos[1] + 1)
enddef

export def SurroundDelete(
    start_pos: list<number>, end_pos: list<number>, target: string
    )
  if target ==# ''
    NotifyWarn('Insufficient arguments for surround delete')
    return
  endif
  const pair = MatchingPair(target)

  const left_pos = FindPos(start_pos, target, pair, true)
  const right_pos = FindPos(end_pos, pair, target, false)
  if PosIsEmpty(left_pos) || PosIsEmpty(right_pos)
    NotifyInfo('Nothing to delete')
    return
  endif

  BufDelText(right_pos, strlen(pair))
  BufDelText(left_pos, strlen(pair))
enddef

export def Surround(
    start_pos: list<number>, end_pos: list<number>, args: list<string>
)
  if empty(args)
    NotifyWarn('No surround action specified')
    return
  endif
  const action = args[0]

  if action ==# 'r' || action ==# 'replace'
    SurroundReplace(start_pos, end_pos,
                    get(args, 1, ''), get(args, 2, ''))
  elseif action ==# 'a' || action ==# 'add'
    SurroundAdd(start_pos, end_pos,
                get(args, 1, ''), get(args, 2, ''))
  elseif action ==# 'd' || action ==# 'delete'
    SurroundDelete(start_pos, end_pos, get(args, 1, ''))
  else
    NotifyWarn('Unknown surround action: ' .. action)
  endif
enddef

# ---------------------------------------------------------------------------
# Plugin help tag generation
# ---------------------------------------------------------------------------

export def PluginUpdateHelp()
  var config_path = fnamemodify(expand('$MYVIMRC'), ':h')
  if config_path ==# '' || config_path ==# '.'
    config_path = expand('~/.config/vim')
  endif
  const plugin_doc_dirs = glob(
            config_path .. '/pack/plugins/*/*/doc', 0, true)
  for dirpath in plugin_doc_dirs
    echo 'Found docs dir: ' .. dirpath
    execute 'helptags ' .. fnameescape(dirpath)
  endfor
enddef

# ---------------------------------------------------------------------------
# Buffer list -> quickfix
# ---------------------------------------------------------------------------

export def BufferQList()
  const buffers = getbufinfo({buflisted: 1})
  var list_contents: list<dict<any>> = []

  for buf in buffers
    if get(buf, 'name', '') ==# ''
      continue
    endif

    var lnum = 1
    var col_ = 0
    for m in getmarklist(buf.bufnr)
      if m.mark ==# "'\""
        lnum = m.pos[1]
        col_ = m.pos[2]
        break
      endif
    endfor

    const lines = getbufline(buf.bufnr, lnum)
    var module_name = buf.name
    if stridx(buf.name, '/') == 0 || stridx(buf.name, '~') == 0
      module_name = fnamemodify(buf.name, ':~:.')
    endif
    module_name ..= ' <' .. buf.bufnr .. '>'

    add(list_contents, {
      bufnr: buf.bufnr,
      module: module_name,
      lnum: lnum,
      col: col_,
      text: get(lines, 0, ''),
    })
  endfor

  setqflist(list_contents, 'r')
  setqflist([], 'a', {title: 'Buffers'})
  copen
enddef

# ---------------------------------------------------------------------------
# Async make (uses job_start)
# ---------------------------------------------------------------------------

def MakeOnOut(state: dict<any>, ch: channel, msg: string)
  add(state.lines, msg)
enddef

def MakeOnExit(state: dict<any>, j: job, status: number)
  const m = 'Command "' .. state.cmd_str
            .. '" completed with code ' .. status
  if status != 0
    NotifyWarn(m)
  else
    NotifyInfo(m)
  endif
  var qf_opts: dict<any> = {title: state.cmd_str, lines: state.lines}
  if state.efm !=# ''
    qf_opts.efm = state.efm
  endif
  setqflist([], ' ', qf_opts)
enddef

export def AsyncMake(args: list<string>)
  const buf_nr = winbufnr(0)
  const efm_raw = getbufvar(buf_nr, '&errorformat', '')
  const efm = type(efm_raw) == v:t_string ? efm_raw : ''
  const mp_raw = getbufvar(buf_nr, '&makeprg', '')
  var mp = type(mp_raw) == v:t_string ? mp_raw : ''
  if mp ==# ''
    mp = 'make'
  endif
  mp = expandcmd(mp)
  var make_cmd = split(mp, ' ')
  for arg in args
    add(make_cmd, expandcmd(arg))
  endfor
  const cmd_str = join(make_cmd, ' ')

  var state: dict<any> = {lines: [], cmd_str: cmd_str, efm: efm}

  NotifyInfo('Running: ' .. cmd_str)
  job_start(make_cmd, {
    out_cb: (ch: channel, msg: string) => MakeOnOut(state, ch, msg),
    err_cb: (ch: channel, msg: string) => MakeOnOut(state, ch, msg),
    exit_cb: (j: job, status: number) => MakeOnExit(state, j, status),
    out_mode: 'nl',
    err_mode: 'nl',
  })
enddef

# ---------------------------------------------------------------------------
# Tabline
# ---------------------------------------------------------------------------

const filetype_to_tabname = {
  fugitive: '[Fugitive]',
  git: '[Git]',
  qf: '[List]',
}

def TabBufName(buf_nr: number): string
  const ft_raw = getbufvar(buf_nr, '&filetype', '')
  const ft = type(ft_raw) == v:t_string ? ft_raw : ''
  if has_key(filetype_to_tabname, ft)
    return filetype_to_tabname[ft]
  endif
  const buf_name = bufname(buf_nr)
  if buf_name ==# ''
    return '[No Name]'
  endif
  return fnamemodify(buf_name, ':t')
enddef

export def Tabline(): string
  var s = ''
  const current_tab_id = tabpagenr()
  const last_tab_nr = tabpagenr('$')
  const win_width = &columns
  const divisor = last_tab_nr > 0 ? last_tab_nr : 1
  const tab_max_width = (win_width / divisor) - 8

  for tab_nr in range(1, last_tab_nr)
    const buf_list = tabpagebuflist(tab_nr)
    const win_nr = tabpagewinnr(tab_nr)
    const buf_nr = buf_list[win_nr - 1]
    const is_selected = (tab_nr == current_tab_id)
            ? '%#TabLineSel#' : '%#TabLine#'
    const selection = '%' .. tab_nr .. 'T'
    var label = '#' .. tab_nr
    if tab_max_width > 4
      var buf_name = TabBufName(buf_nr)
      if strlen(buf_name) > tab_max_width
        buf_name = strpart(buf_name, 0, tab_max_width - 1) .. '…'
      endif
      label ..= ' ' .. buf_name
    endif
    s ..= is_selected .. selection .. '  ' .. label .. '  '
  endfor

  s ..= '%#TabLineFill#%T'
  return s
enddef

# ---------------------------------------------------------------------------
# Yank highlight (TextYankPost autocmd helper)
# ---------------------------------------------------------------------------

var on_yank_match_id = 0
var on_yank_match_win = 0
var on_yank_timer = 0

def OnYankClear(_t: number = 0)
  if on_yank_match_id > 0
    if on_yank_match_win > 0
        && win_id2tabwin(on_yank_match_win) != [0, 0]
      try
        matchdelete(on_yank_match_id, on_yank_match_win)
      catch
      endtry
    endif
    on_yank_match_id = 0
    on_yank_match_win = 0
  endif
  on_yank_timer = 0
enddef

export def OnYank()
  if !exists('v:event') || get(v:event, 'operator', '') !=# 'y'
    return
  endif
  if empty(get(v:event, 'regcontents', []))
    return
  endif

  if on_yank_timer > 0
    timer_stop(on_yank_timer)
  endif
  OnYankClear()

  const start_pos = getpos("'[")
  const end_pos = getpos("']")
  if start_pos[1] == 0 || end_pos[1] == 0
    return
  endif

  const s_lnum = start_pos[1]
  const s_col = start_pos[2]
  const e_lnum = end_pos[1]
  const e_col = end_pos[2]

  var pos_list: list<any> = []
  for lnum in range(s_lnum, e_lnum)
    if lnum == s_lnum && lnum == e_lnum
      add(pos_list, [lnum, s_col, e_col - s_col + 1])
    elseif lnum == s_lnum
      add(pos_list, [lnum, s_col, 9999])
    elseif lnum == e_lnum
      add(pos_list, [lnum, 1, e_col])
    else
      add(pos_list, lnum)
    endif
    if len(pos_list) >= 8
      break
    endif
  endfor

  if empty(pos_list)
    return
  endif

  on_yank_match_win = win_getid()
  on_yank_match_id = matchaddpos('IncSearch', pos_list, 10, -1,
                                  {window: on_yank_match_win})
  on_yank_timer = timer_start(150, OnYankClear)
enddef

# ---------------------------------------------------------------------------
# CopyPath user command
# ---------------------------------------------------------------------------

export def CopyPath(args: list<string>)
  var expansion = '%:.'
  var pos_mode = ''
  for v in args
    if v ==# 'f'
      expansion = '%:p'
    elseif v ==# 'l'
      pos_mode = ':line'
    elseif v ==# 'c'
      pos_mode = ':line:col'
    elseif v ==# '+'
      pos_mode = '+line'
    endif
  endfor

  const path = expand(expansion)
  const row = line('.')
  const col_nr = col('.')
  var pos_str = ''
  if pos_mode ==# ':line'
    pos_str = ':' .. row
  elseif pos_mode ==# ':line:col'
    pos_str = ':' .. row .. ':' .. col_nr
  elseif pos_mode ==# '+line'
    pos_str = ' +' .. row
  endif
  const contents = path .. pos_str

  setreg('+', contents)
  echo contents
enddef

# ---------------------------------------------------------------------------
# Grep via git
# ---------------------------------------------------------------------------

export def GitGrep(args: string)
  const saved = &l:grepprg
  try
    setlocal grepprg=git\ grep\ -n\ --column
    execute 'lgrep ' .. args
  finally
    &l:grepprg = saved
  endtry
enddef

# ---------------------------------------------------------------------------
# Toggle autosave (used by :ToggleAutoSave)
# ---------------------------------------------------------------------------

export def ToggleAutoSave()
  g:disableautosave = !get(g:, 'disableautosave', 0)
  echo '[Auto save] ' .. (g:disableautosave ? 'OFF' : 'ON')
enddef

# ---------------------------------------------------------------------------
# Autosave callback (used by autocmd)
# ---------------------------------------------------------------------------

export def Autosave()
  if get(g:, 'disableautosave', 0) || expand('%:h') ==# ''
    return
  endif
  if &readonly
    return
  endif
  silent! update
enddef

# ---------------------------------------------------------------------------
# Misc helpers used from autocmds and the plugin loader
# ---------------------------------------------------------------------------

export def MkdirForBuffer()
  const dir = expand('<afile>:p:h')
  if dir !=# '' && !isdirectory(dir)
    mkdir(dir, 'p')
  endif
enddef

export def SetupQuickfixWindow()
  setlocal nowrap
  setlocal cursorline
  nnoremap <buffer> <silent> <leader>q <Cmd>cclose<CR>
  nnoremap <buffer> <silent> <leader>l <Cmd>lclose<CR>
  nnoremap <buffer> <silent> g<CR> <C-w><CR><C-w>T
enddef

export def SetupPerProjectViminfo()
  var cwd = getcwd()
  const git_dir = finddir('.git', cwd .. ';')
  if git_dir !=# ''
    cwd = fnamemodify(git_dir, ':p:h:h')
  endif
  const base = expand('~/.cache/vim/project_viminfo')
  if !isdirectory(base)
    mkdir(base, 'p')
  endif
  &viminfofile = base .. '/' .. sha256(cwd)
enddef

export def ConfigureVimwiki()
  g:vimwiki_list = [{
    path: '~/wiki/',
    syntax: 'markdown',
    ext: 'md',
    diary_rel_path: 'Notes',
  }]
  g:vimwiki_global_ext = 0
  g:vimwiki_ext2syntax = {}
  g:vimwiki_auto_header = 1
enddef

# ---------------------------------------------------------------------------
# Lazy plugin loader
# ---------------------------------------------------------------------------

var plugin_specs: dict<dict<any>> = {}
var loaded_plugins: dict<bool> = {}
var configs_run: dict<bool> = {}
var shim_cleared: dict<bool> = {}
var plugin_key_map: list<dict<any>> = []

def PluginKey(spec: dict<any>): string
  return type(spec.name) == v:t_string
        ? spec.name
        : spec.name[0]
enddef

def PluginRunConfig(spec: dict<any>, key: string)
  if get(configs_run, key, false) || !has_key(spec, 'config')
    return
  endif
  try
    call(spec.config, [])
  catch
    const label = type(spec.name) == v:t_string
              ? spec.name
              : join(spec.name, ', ')
    NotifyWarn(
      'Failed to config plugin ' .. label .. ': ' .. v:exception)
  endtry
  configs_run[key] = true
enddef

def PluginUnloadShims(spec: dict<any>, key: string)
  if get(shim_cleared, key, false)
    return
  endif
  if has_key(spec, 'cmd')
    for c in spec.cmd
      try
        execute 'delcommand ' .. c
      catch
      endtry
    endfor
  endif
  if has_key(spec, 'keys')
    for keyspec in spec.keys
      if !has_key(keyspec, 'rhs') || keyspec.rhs ==# ''
        execute 'silent! nunmap ' .. keyspec.lhs
      endif
    endfor
  endif
  shim_cleared[key] = true
enddef

def PluginLoadPkg(spec: dict<any>, key: string)
  if get(loaded_plugins, key, false)
    return
  endif
  if type(spec.name) == v:t_string
    execute 'packadd ' .. spec.name
  else
    for nm in spec.name
      execute 'packadd ' .. nm
    endfor
  endif
  loaded_plugins[key] = true
enddef

export def PluginLoad(key: string)
  if !has_key(plugin_specs, key)
    return
  endif
  const spec = plugin_specs[key]
  PluginUnloadShims(spec, key)
  PluginLoadPkg(spec, key)
  PluginRunConfig(spec, key)
enddef

def TranslateKeys(s: string): string
  var work = s
  if exists('g:mapleader')
    work = substitute(
        work, '\c<leader>',
        escape(g:mapleader, '\&~'), 'g'
    )
  endif
  if exists('g:maplocalleader')
    work = substitute(
        work, '\c<localleader>',
        escape(g:maplocalleader, '\&~'), 'g'
    )
  endif
  try
    var escaped = escape(work, '"\')
    escaped = substitute(escaped, '<', '\\<', 'g')
    return eval('"' .. escaped .. '"')
  catch
    return work
  endtry
enddef

def PluginRunKey(idx: number)
  if idx < 0 || idx >= len(plugin_key_map)
    return
  endif
  const entry = plugin_key_map[idx]
  PluginLoad(entry.key)
  const feed = entry.rhs !=# '' ? entry.rhs : entry.lhs
  feedkeys(TranslateKeys('<ignore>' .. feed), 'm')
enddef

def PluginRunCmd(
    key: string, cmd: string, bang: number, mods: string,
    line1: number, line2: number, range_count: number, args: string
)
  PluginLoad(key)

  var range_str = ''
  if range_count == 1
    range_str = string(line1)
  elseif range_count == 2
    range_str = line1 .. ',' .. line2
  endif
  const bang_str = bang != 0 ? '!' : ''
  const mods_str = mods !=# '' ? (mods .. ' ') : ''
  execute mods_str .. range_str .. cmd .. bang_str .. ' ' .. args
enddef

export def PluginRegister(spec: dict<any>)
  const key = PluginKey(spec)
  plugin_specs[key] = spec

  if !get(spec, 'config_lazy', false)
    PluginRunConfig(spec, key)
  endif

  if has_key(spec, 'cmd')
    for c in spec.cmd
      execute printf(
        'command! -bang -range=-1 -nargs=* %s '
          .. 'call %sPluginRunCmd(%s, %s, <bang>0, '
          .. '"<mods>", <line1>, <line2>, <range>, <q-args>)',
        c, SID, string(key), string(c))
    endfor
  endif

  if has_key(spec, 'keys')
    for keyspec in spec.keys
      const lhs = keyspec.lhs
      const rhs = get(keyspec, 'rhs', '')
      const idx = len(plugin_key_map)
      add(
          plugin_key_map,
          {key: key, lhs: lhs, rhs: rhs}
      )
      execute printf(
        'nnoremap <silent> %s <Cmd>call %sPluginRunKey(%d)<CR>',
        lhs, SID, idx
      )
    endfor
  endif
enddef
