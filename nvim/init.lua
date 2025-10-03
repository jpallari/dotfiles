--
-- Appearance
--
vim.opt.number = false
vim.opt.relativenumber = false
vim.opt.showmode = true
vim.opt.laststatus = 2
vim.opt.breakindent = true
vim.opt.signcolumn = 'yes'
vim.opt.list = true
vim.opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }
vim.opt.cursorline = false
vim.opt.wrap = true
vim.opt.termguicolors = true
vim.opt.shortmess:append 'I'
vim.opt.title = false
vim.opt.winborder = 'single'
vim.cmd.colorscheme 'lunaperche'
vim.cmd.hi 'Normal guibg=NONE guifg=NONE ctermbg=NONE ctermfg=NONE'
vim.cmd.hi 'Pmenu guibg=Blue'
vim.cmd.hi 'NormalFloat guibg=NvimDarkGray2'
vim.cmd.hi 'VertSplit guibg=NvimDarkGray2'

--
-- UI
--
vim.opt.updatetime = 250
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.inccommand = 'split' -- preview subtitutions
vim.opt.scrolloff = 5        -- min screen lines
vim.opt.errorbells = false
vim.opt.ignorecase = true
vim.opt.wildignorecase = true
vim.opt.wildignore:append({ '*.o', '*.obj', '*.pyc', '*.class', '*.jar' })
vim.opt.wildmode = 'longest:full:lastused'
vim.opt.foldcolumn = '0'
vim.opt.foldtext = ''
vim.opt.foldnestmax = 3
vim.opt.foldlevelstart = 99
vim.opt.foldmethod = 'indent'
vim.opt.completeopt = { 'menuone', 'noselect', 'popup' }
vim.opt.pumheight = 10

--
-- Controls
--
vim.opt.mouse = 'a'
vim.opt.clipboard = 'unnamedplus' -- global clipboard

--
-- File management
--
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv 'HOME' .. '/.vim/undodir'
vim.opt.undofile = true
vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 0
vim.g.netrw_altfile = 1
vim.g.netrw_keepdir = 0
vim.g.netrw_winsize = 30
vim.g.netrw_localcopydircmd = 'cp -r'
vim.g.netrw_localmkdir = 'mkdir -p'
vim.g.netrw_localrmdir = 'rm -r'

--
-- Search
--
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = false
vim.opt.gdefault = true
vim.opt.infercase = true
vim.opt.incsearch = true
if vim.fn.executable('rg') then
  vim.opt.grepprg = 'rg --vimgrep --hidden --smart-case --iglob "!**/.git/*"'
  vim.opt.grepformat = '%f:%l:%c:%m,%f:%l:%m'
end

--
-- Editing
--
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.joinspaces = false

--
-- System
--
vim.opt.exrc = true
vim.g.editorconfig = true
vim.opt.shadafile = (function()
  local data = vim.fn.stdpath('data')
  local cwd = vim.fn.getcwd()
  cwd = vim.fs.root(cwd, '.git') or cwd
  local cwd_b64 = vim.base64.encode(cwd)
  local file = vim.fs.joinpath(data, 'project_shada', cwd_b64)
  vim.fn.mkdir(vim.fs.dirname(file), 'p')
  return file
end)()

--
-- Diagnostics
--
vim.diagnostic.config({
  underline = false,
  virtual_text = {
    current_line = true,
    prefix = '■ ',
  },
  update_in_insert = false,
})

--
-- Custom functions
--
function SetIndent(level)
  if level == nil then
    level = 4
  end
  level = tonumber(level)
  vim.opt_local.expandtab = true
  vim.opt_local.tabstop = level
  vim.opt_local.softtabstop = level
  vim.opt_local.shiftwidth = level
end

function SetIndentTab(level)
  if level == nil then
    level = 4
  end
  level = tonumber(level)
  vim.opt_local.expandtab = false
  vim.opt_local.tabstop = level
  vim.opt_local.softtabstop = 0
  vim.opt_local.shiftwidth = level
end

function FindFiles(pattern, cmd_name)
  local rg_cmd = {
    'rg', '--files',
    '--hidden',
    '--color=never',
    '--smart-case',
    '--iglob',
    '!**/.git/*',
    '--iglob',
    '**/' .. pattern .. '*'
  }
  local find_cmd = {
    'find', '.',
    '-type', 'f',
    '-ipath',
    pattern,
  }
  local git_cmd = {
    'git',
    'ls-files',
    pattern
  }
  local cmd

  if cmd_name == 'rg' then
    if vim.fn.executable('rg') == 1 then
      cmd = rg_cmd
    else
      vim.notify('Command "rg" not found', vim.log.levels.ERROR)
      return
    end
  elseif cmd_name == 'find' then
    cmd = find_cmd
  elseif cmd_name == 'git' then
    cmd = git_cmd
  elseif cmd_name == nil then
    if vim.fn.executable('rg') == 1 then
      cmd = rg_cmd
    else
      cmd = find_cmd
    end
  else
    vim.notify('Unknown command: ' .. cmd_name, vim.log.levels.ERROR)
    return
  end

  local res = vim.system(cmd):wait()
  if res.code ~= 0 then
    vim.notify('Find failed: ' .. res.stderr, vim.log.levels.ERROR)
    return
  end

  local list_contents = {}
  local lines = vim.split(res.stdout, '\n', { plain = true, trimempty = true })
  for i = 1, #lines do
    local line = lines[i]
    table.insert(list_contents, {
      filename = line,
      lnum = 1,
      col = 1,
      text = '',
    })
  end

  vim.fn.setloclist(0, list_contents, 'r')
  vim.fn.setloclist(0, {}, 'a', { title = 'Find: ' .. pattern })
  if lines[1] then
    vim.cmd.lfirst()
  end
end

--
-- Custom commands
--
do
  local cmd = vim.api.nvim_create_user_command
  cmd('SetIndent', function(args)
    SetIndent(args.args)
  end, { nargs = '*', desc = 'Set indentation level' })
  cmd('SetIndentTab', function(args)
    SetIndentTab(args.args)
  end, { nargs = '*', desc = 'Set tab indentation level' })
  cmd('ToggleAutoSave', function()
    vim.g.disableautosave = not vim.g.disableautosave
    print('[Auto save] ' .. (vim.g.disableautosave and 'OFF' or 'ON'))
  end, { nargs = 0, desc = 'Toggle auto save' })
  cmd('TrimWhitespace', function()
    vim.cmd '%s/\\s\\+$//e'
  end, { desc = 'Trim trailing whitespace characters' })
  cmd('CopyFullPath', function()
    local path = vim.fn.expand('%:p')
    vim.fn.setreg('+', path)
    print('Path: ' .. path)
  end, { desc = 'Copy full path to clipboard' })
  cmd('CopyRelPath', function()
    local path = vim.fn.expand('%:.')
    vim.fn.setreg('+', path)
    print('Path: ' .. path)
  end, { desc = 'Copy relative path to clipboard' })
  cmd('FindFiles', function(args)
    FindFiles(args.args)
  end, { nargs = 1, desc = 'Find files by file name' })
  cmd('FindGitFiles', function(args)
    FindFiles(args.args, 'git')
  end, { nargs = 1, desc = 'Find files by file name from git' })
  cmd('GitGrep', function(args)
    local grepprg = vim.opt_local.grepprg
    vim.opt_local.grepprg = 'git grep -n --column'
    vim.cmd('lgrep ' .. args.args)
    vim.opt_local.grepprg = grepprg
  end, { nargs = 1, desc = 'Grep files from git' })
end

--
-- Autocommands
--  See `:help lua-guide-autocommands`
--
do
  local autocmd = vim.api.nvim_create_autocmd
  local augroup = vim.api.nvim_create_augroup('dotfile', { clear = true })

  -- Highlight when yanking (copying) text
  autocmd('TextYankPost', {
    desc = 'Highlight when yanking (copying) text',
    group = augroup,
    callback = function()
      vim.highlight.on_yank()
    end,
  })

  -- Create directories when saving files
  autocmd({ 'BufWritePre' }, {
    desc = 'MkDirP',
    group = augroup,
    callback = function()
      local dir = vim.fn.expand('<afile>:p:h')
      if vim.fn.isdirectory(dir) == 0 then
        vim.fn.mkdir(dir, 'p')
      end
    end,
  })

  -- Autosave
  autocmd({ 'CursorHold', 'TextChanged', 'InsertLeave' }, {
    desc = 'Autosave',
    group = augroup,
    callback = function()
      if vim.g.disableautosave or vim.fn.expand '%h' == '' then
        return
      end

      local buf = vim.api.nvim_win_get_buf(0)
      if vim.bo[buf].readonly then
        return
      end

      vim.cmd 'silent! update'
    end,
  })

  -- LSP
  autocmd('LspAttach', {
    desc = 'LSP',
    group = augroup,
    callback = function(event)
      local map = function(keys, func, desc)
        vim.keymap.set('n', keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
      end

      -- LSP mappings
      map('K', vim.lsp.buf.hover, 'Hover Documentation')
      map('grD', vim.lsp.buf.declaration, 'Goto declaration')
      map('<leader>ca', vim.lsp.buf.code_action, 'Code action')
      map('<leader>cf', vim.lsp.buf.format, 'Code format')
      map('<leader>cl', vim.lsp.codelens.run, 'Code lens')
      map('<leader>cs', vim.lsp.buf.signature_help, 'Code signature')
      map('<leader>cr', vim.lsp.buf.rename, 'Code Rename')
      map('<leader>tdh', function()
        vim.g.lsp_doc_hl_enabled = not vim.g.lsp_doc_hl_enabled
      end, 'Toggle document highlight')

      -- Highlight references under cursor
      local client = vim.lsp.get_client_by_id(event.data.client_id)
      if client and client.server_capabilities.documentHighlightProvider then
        vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
          buffer = event.buf,
          callback = function()
            if vim.g.lsp_doc_hl_enabled then
              vim.lsp.buf.document_highlight()
            end
          end,
        })

        vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
          buffer = event.buf,
          callback = vim.lsp.buf.clear_references,
        })
      end

      -- Completion
      if client and client:supports_method('textDocument/completion') then
        vim.lsp.completion.enable(true, client.id, event.buf, { autotrigger = false })
        vim.keymap.set('i', '<C-space>', vim.lsp.completion.get, { desc = 'Trigger completion' })
      end

      -- Inlay hints
      if client and client.server_capabilities.inlayHintProvider and vim.lsp.inlay_hint then
        map('<leader>tih', function()
          vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
        end, 'Toggle inlay hints')
      end
    end,
  })
end

--
-- File types
--
vim.filetype.add({
  extension = {
    tf = 'terraform',
  },
})

--
-- Keymaps
--
do
  local mapk = vim.keymap.set

  -- Leader key
  vim.g.mapleader = ' '
  vim.g.maplocalleader = ' '

  -- Set highlight on search, but clear on pressing <Esc> in normal mode
  mapk('n', '<Esc>', '<cmd>nohlsearch<CR>')

  -- Movement
  mapk('n', 'j', 'gj', { noremap = true })
  mapk('n', 'k', 'gk', { noremap = true })

  -- Diagnostic
  mapk('n', '<leader>dd', vim.diagnostic.open_float, { desc = 'Diagnostic error messages' })
  mapk('n', '<leader>dl', vim.diagnostic.setloclist, { desc = 'Diagnostic location list' })
  mapk('n', '<leader>dq', function()
    vim.diagnostic.setqflist()
  end, { desc = 'Diagnostic quickfix list' })
  mapk('n', '<leader>de', function()
    vim.diagnostic.setqflist { severity = vim.diagnostic.severity.ERROR }
  end, { desc = 'Diagnostic errors quickfix list' })
  mapk('n', '<leader>dw', function()
    vim.diagnostic.setqflist { severity = vim.diagnostic.severity.WARN }
  end, { desc = 'Diagnostic warnings quickfix list' })

  -- Window management
  mapk('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
  mapk('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
  mapk('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
  mapk('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })
  mapk('n', '<C-left>', ':vertical resize +5<CR>', { desc = 'Increase window size horizontally' })
  mapk('n', '<C-right>', ':vertical resize -5<CR>', { desc = 'Decrease window size horizontally' })
  mapk('n', '<C-up>', ':resize +5<CR>', { desc = 'Increase window size vertically' })
  mapk('n', '<C-down>', ':resize -5<CR>', { desc = 'Decrease window size vertically' })
  for i = 1, 9 do
    mapk('n', '<leader>' .. i, '<cmd>' .. i .. 'tabnext<cr>', { desc = 'Go to tab #' .. i })
  end

  -- Exit terminal mode in the builtin terminal with a shortcut that is a bit easier
  -- for people to discover. Otherwise, you normally need to press <C-\><C-n>, which
  -- is not what someone will guess without a bit more experience.
  --
  -- NOTE: This won't work in all terminal emulators/tmux/etc. Try your own mapping
  -- or just use <C-\><C-n> to exit terminal mode
  mapk('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

  -- Editing
  mapk('i', '<C-space>', '<C-n>', { desc = 'Trigger completion' })
  mapk('i', '<M-Backspace>', '<C-w>', { desc = 'Delete word' })
  mapk('c', '<M-Backspace>', '<C-w>', { desc = 'Delete word' })
  mapk('v', '<leader>c', '"+y', { desc = 'Copy to system clipboard' })
  mapk('n', '<leader>v', '"+p', { desc = 'Paste from system clipboard' })
  mapk('n', 'x', '"_dl', { noremap = true, desc = 'Delete character' })
  mapk('n', 'X', '"_dh', { noremap = true, desc = 'Backspace' })
  mapk('n', '<leader>x', '"_d', { noremap = true, desc = 'Delete (w/o yank)' })
  mapk('n', '<leader>X', '"_D', { noremap = true, desc = 'Delete to EOL (w/o yank)' })
  mapk('n', 'Y', 'y$', { noremap = true, desc = 'Yank the rest of the line' })
  mapk('n', '<C-/>', 'gcc', { remap = true, desc = 'Comment current line' })
  mapk('n', '<C-_>', 'gcc', { remap = true, desc = 'Comment current line' })
  mapk('v', '<C-/>', 'gc', { remap = true, desc = 'Comment selected lines' })
  mapk('v', '<C-_>', 'gc', { remap = true, desc = 'Comment selected lines' })

  -- Keep selection on indentation
  mapk('v', '<', '<gv', { desc = 'Lower indent and keep selection' })
  mapk('v', '>', '>gv', { desc = 'Increase indent and keep selection' })

  -- Move selected lines up/down
  mapk('v', 'J', ":m '>+1<CR>gv=gv", { desc = 'Move lines down' })
  mapk('v', 'K', ":m '<-2<CR>gv=gv", { desc = 'Move lines up' })

  -- Files and buffers
  mapk('n', '<leader>e', ":e <C-R>=expand('%:p:h') . '/' <cr>", { desc = 'Open file from current buffer directory' })
  mapk('n', '\\', '<cmd>Lexplore!<cr>', { desc = 'File explorer' })
  mapk('n', '<leader>\\', '<cmd>Lexplore! %:p:h<cr>', { desc = 'File explorer in current dir' })
  mapk('n', '<leader><leader>', '<cmd>ls<cr>:b ', { desc = 'Select buffer' })
  mapk('n', '<leader>m', '<cmd>marks \'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<cr>', { desc = 'View marks' })
  mapk('n', '<leader>ff', ':FindFiles ', { desc = 'Find files' })
  mapk('n', '<leader>fg', ':FindGitFiles ', { desc = 'Find files in git' })
  mapk('n', '<leader>gg', ':GitGrep ', { desc = 'Grep using git' })
  mapk('n', '<leader>gf', ':lgrep ', { desc = 'Grep' })

  -- Quickfix and location list
  mapk('n', '<leader>q', '<cmd>copen<cr>', { desc = 'Open quickfix list' })
  mapk('n', '<leader>l', '<cmd>lopen<cr>', { desc = 'Open location list' })

  -- Command mode navigation
  mapk('c', '<C-f>', '<right>', { desc = 'Move cursor right' })
  mapk('c', '<C-b>', '<left>', { desc = 'Move cursor left' })
  mapk('c', '<M-f>', '<c-right>', { desc = 'Move cursor one word right' })
  mapk('c', '<M-b>', '<c-left>', { desc = 'Move cursor one word left' })
  mapk('c', '<M-right>', '<c-right>', { desc = 'Move cursor one word right' })
  mapk('c', '<M-left>', '<c-left>', { desc = 'Move cursor one word left' })

  -- Toggles
  mapk('n', '<leader>th', '<cmd>setlocal hlsearch!<cr>', { desc = 'Toggle search hilight' })
  mapk('n', '<leader>tp', '<cmd>set paste!<cr>', { desc = 'Toggle paste mode' })
  mapk('n', '<leader>tn', '<cmd>set number!<cr>', { desc = 'Toggle numbers' })
  mapk('n', '<leader>tr', '<cmd>set relativenumber!<cr>', { desc = 'Toggle relative numbers' })
  mapk('n', '<leader>tw', '<cmd>setlocal wrap!<cr>', { desc = 'Toggle line wrapping' })
  mapk('n', '<leader>tcl', '<cmd>setlocal cursorline!<cr>', { desc = 'Toggle cursor line' })
  mapk('n', '<leader>tcc', '<cmd>setlocal cursorcolumn!<cr>', { desc = 'Toggle cursor column' })
  mapk('n', '<leader>tas', '<cmd>ToggleAutoSave<cr>', { desc = 'Toggle autosave' })
end

--
-- LSP
--
do
  local lsp_server_configs = {
    bashls = {},
    clangd = {},
    gopls = {
      settings = {
        gopls = {
          gofumpt = true,
          buildFlags = { '-tags=integration' },
        },
      },
    },
    lua_ls = {
      settings = {
        Lua = {
          diagnostics = {
            globals = { 'vim' }
          },
          completion = {
            callSnippet = 'Replace',
          },
          window = {
            progressBar = false,
            statusBar = false,
          },
        },
      },
    },
    rust_analyzer = {},
    terraformls = {},
    ts_ls = {
      init_options = {
        preferences = {
          disableSuggestions = true,
        },
      },
    },
    zls = {},
  }

  for server_name, server in pairs(lsp_server_configs) do
    vim.lsp.config(server_name, server)
    vim.lsp.enable(server_name)
  end
end

--
-- Plugins
--
do
  local plugins = {
    {
      name = 'vim-fugitive',
      cmd = { 'Git', 'Gedit', 'Ge', 'Gclog', },
      keys = {
        { '<leader>GG', '<cmd>Ge :<cr>',      desc = 'Git status' },
        { '<leader>Gg', '<cmd>Ge :<cr>',      desc = 'Git status' },
        { '<leader>Gs', '<cmd>Git<cr>',       desc = 'Git status' },
        { '<leader>GB', '<cmd>Git blame<cr>', desc = 'Git blame' },
        { '<leader>Gl', '<cmd>Gclog<cr>',     desc = 'Git log in quickfix list' },
      },
    },
    {
      name = 'nvim-treesitter',
      config = function()
        require('nvim-treesitter.install').prefer_git = true
        require('nvim-treesitter.configs').setup({
          ensure_installed = { 'git_rebase' },
          auto_install = true,
          highlight = {
            enable = true,
            additional_vim_regex_highlighting = false,
          },
          indent = { enable = true, disable = { 'c', 'cpp', 'yaml' } },
        })
      end,
    },
    {
      name = 'vimwiki',
      cmd = { 'VimwikiIndex', 'VimwikiMakeDiaryNote' },
      keys = {
        { '<leader>ww', desc = 'Wiki: Open default index file' },
        { '<leader>ws', desc = 'Wiki: Select and open index file' },
        { '<leader>wd', desc = 'Wiki: Create diary note' },
      },
      config = function()
        vim.g.vimwiki_list = {
          {
            path = '~/wiki/',
            syntax = 'markdown',
            ext = 'md',
            diary_rel_path = 'Notes',
          },
        }
        vim.g.vimwiki_global_ext = 0
        vim.g.vimwiki_ext2syntax = vim.empty_dict()
        vim.g.vimwiki_auto_header = 1
      end,
    }
  }

  for _, plugin in pairs(plugins) do
    local plugin_loaded = false
    local shim_unloaded = false

    local function load_plug()
      if not plugin_loaded then
        vim.cmd.packadd(plugin.name)
        plugin_loaded = true
      end
    end
    local function unload_shim()
      if not shim_unloaded then
        for _, cmd in pairs(plugin.cmd) do
          vim.api.nvim_del_user_command(cmd)
        end
        for _, key in pairs(plugin.keys) do
          if not key[2] then
            pcall(vim.keymap.del, 'n', key[1], { buffer = nil })
          end
        end
        shim_unloaded = true
      end
    end

    if plugin.config then
      local ok, err = pcall(plugin.config)
      if not ok then
        vim.notify(
          'Failed to config plugin "' .. plugin.name .. '": ' .. err,
          vim.log.levels.WARN
        )
      end
    end

    if plugin.cmd then
      for _, cmd in pairs(plugin.cmd) do
        vim.api.nvim_create_user_command(cmd, function(event)
          local command = {
            cmd = cmd,
            bang = event.bang or nil,
            mods = event.smods,
            args = event.fargs,
            count = event.count >= 0 and event.range == 0 and event.count or nil,
          }
          if event.range == 1 then
            command.range = { event.line1 }
          elseif event.range == 2 then
            command.range = { event.line1, event.line2 }
          end

          unload_shim()
          load_plug()

          local info = vim.api.nvim_get_commands({})[cmd] or vim.api.nvim_buf_get_commands(0, {})[cmd]
          command.nargs = info.nargs
          if event.args and event.args ~= "" and info.nargs and info.nargs:find("[1?]") then
            command.args = { event.args }
          end
          vim.cmd(command)
        end, { bang = true, range = true, nargs = '*' })
      end
    end

    if plugin.keys then
      for _, key in pairs(plugin.keys) do
        local lhs = key[1]
        local rhs = key[2]
        vim.keymap.set('n', lhs, function()
          unload_shim()
          load_plug()
          local feed_key = vim.api.nvim_replace_termcodes('<Ignore>' .. (rhs or lhs), true, true, true)
          vim.api.nvim_feedkeys(
            feed_key,
            'i',
            false
          )
        end, { desc = key.desc })
      end
    end
  end
end

-- vim: ts=2 sts=2 sw=2 et
