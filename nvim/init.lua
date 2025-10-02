--
-- Appearance
--
vim.g.have_nerd_font = true
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
vim.opt.timeoutlen = 300 -- Faster popup
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
    vim.cmd.find(lines[1])
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
  cmd('FF', function(args)
    FindFiles(args.args)
  end, { nargs = 1, desc = 'Find files by file name' })
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
do
  vim.filetype.add({
    extension = {
      tf = 'terraform',
    },
  })
end

--
-- Keymaps
--  See `:help vim.keymap.set()`
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
  mapk('n', '\\', '<cmd>Ex<cr>', { desc = 'File explorer' })

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
  mapk('n', '<leader>tcc', '<cmd>setlocal cursorcolumn!<cr>', { desc = 'Toggle cursor line' })
  mapk('n', '<leader>tas', '<cmd>ToggleAutoSave<cr>', { desc = 'Toggle autosave' })
end

--
-- LSP
--
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

--
-- Plugin manager
--    See `:help lazy.nvim.txt` or https://github.com/folke/lazy.nvim for more info
--

-- Plugin setup
local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
  vim.fn.system { 'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo, lazypath }
end ---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)

-- List of plugins to install
local lazy_plugins = {
  {
    -- Async make
    'tpope/vim-dispatch',
    cmd = { 'Make', 'Dispatch', 'Start' },
    keys = {
      { '<leader>cm', '<cmd>Make<cr>', desc = 'Run make (async)' },
      { '<leader>cM', ':Make ',        desc = 'Run make command (async)' },
    },
  },

  {
    -- Detect tabstop and shiftwidth automatically
    'tpope/vim-sleuth',
    event = { 'BufReadPost', 'BufNewFile' },
  },

  {
    -- Git UI
    'tpope/vim-fugitive',
    cmd = { 'Git', 'Gedit', 'Ge' },
    keys = {
      { '<leader>GG', '<cmd>Ge :<cr>',      desc = 'Git status' },
      { '<leader>Gg', '<cmd>Ge :<cr>',      desc = 'Git status' },
      { '<leader>Gs', '<cmd>Git<cr>',       desc = 'Git status' },
      { '<leader>GB', '<cmd>Git blame<cr>', desc = 'Git blame' },
      { '<leader>Gl', '<cmd>Gclog<cr>',     desc = 'Git log in quickfix list' },
    },
  },

  {
    -- Quick jump between files
    'ThePrimeagen/harpoon',
    branch = 'harpoon2',
    keys = {
      { '<leader>hh', desc = 'Harpoon quick menu' },
      { '<leader>ha', desc = 'Harpoon add buffer' },
      { '<leader>hx', desc = 'Harpoon remove buffer' },
      { '<leader>1',  desc = 'Harpoon buffer 1' },
      { '<leader>2',  desc = 'Harpoon buffer 2' },
      { '<leader>3',  desc = 'Harpoon buffer 3' },
      { '<leader>4',  desc = 'Harpoon buffer 4' },
      { '<leader>5',  desc = 'Harpoon buffer 5' },
      { '<leader>6',  desc = 'Harpoon buffer 6' },
      { '<leader>7',  desc = 'Harpoon buffer 7' },
      { '<leader>8',  desc = 'Harpoon buffer 8' },
      { '<leader>9',  desc = 'Harpoon buffer 9' },
      { '[h',         desc = 'Harpoon previous buffer' },
      { ']h',         desc = 'Harpoon next buffer' },
    },
    requires = { { 'nvim-lua/plenary.nvim' } },
    config = function()
      local harpoon = require 'harpoon'
      harpoon:setup {
        settings = {
          save_on_toggle = true,
          sync_on_ui_close = true,
        },
      }

      local mapk = vim.keymap.set
      mapk('n', '<leader>hh', function()
        harpoon.ui:toggle_quick_menu(harpoon:list())
      end, { desc = 'Harpoon quick menu' })
      mapk('n', '<leader>ha', function()
        harpoon:list():add()
      end, { desc = 'Harpoon add buffer' })
      mapk('n', '<leader>hx', function()
        harpoon:list():remove()
      end, { desc = 'Harpoon remove buffer' })
      mapk('n', '[h', function()
        harpoon:list():prev { ui_nav_wrap = true }
      end, { desc = 'Harpoon previous buffer' })
      mapk('n', ']h', function()
        harpoon:list():next { ui_nav_wrap = true }
      end, { desc = 'Harpoon next buffer' })

      for i = 1, 9 do
        mapk('n', '<leader>' .. i, function()
          harpoon:list():select(i)
        end, { desc = 'Harpoon buffer ' .. i })
      end
    end,
  },

  {
    -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    cmd = { 'LspInfo', 'LspStart', 'Mason', },
    keys = {
      { '<leader>cS', '<cmd>LspStart<cr>',   desc = 'Start LSP' },
      { '<leader>cR', '<cmd>LspRestart<cr>', desc = 'Restart LSP' },
      { '<leader>cQ', '<cmd>LspStop<cr>',    desc = 'Quit LSP' },
    },
    dependencies = {
      { 'mason-org/mason.nvim', opts = {} },
    },
    config = function()
      for server_name, server in pairs(lsp_server_configs) do
        vim.lsp.config(server_name, server)
        vim.lsp.enable(server_name)
      end
    end,
  },

  {
    -- Debug Adapter Protocol (DAP)
    'mfussenegger/nvim-dap',
    keys = {
      { '<leader>Ds',  desc = 'Debug: Start/continue' },
      { '<leader>Di',  desc = 'Debug: Step into' },
      { '<leader>Do',  desc = 'Debug: Step over' },
      { '<leader>DO',  desc = 'Debug: Step Out' },
      { '<leader>Db',  desc = 'Debug: Toggle breakpoint' },
      { '<leader>Drl', desc = 'Debug: Run last' },
      { '<leader>DB',  desc = 'Debug: Set Breakpoint' },
      { '<leader>Drr', desc = 'Debug: See last session result' },
      { '<leader>Dc',  desc = 'Debug: Run to cursor' },
    },
    dependencies = {
      'rcarriga/nvim-dap-ui',
      'nvim-neotest/nvim-nio',
      'mason-org/mason.nvim',
      'jay-babu/mason-nvim-dap.nvim',

      -- Debuggers
      'leoluz/nvim-dap-go', -- Go
    },
    config = function()
      local dap = require 'dap'
      local dapui = require 'dapui'

      require('mason-nvim-dap').setup {
        automatic_setup = true,
        handlers = {},
        automatic_installation = true,
        ensure_installed = {},
      }
      ---@diagnostic disable-next-line: missing-fields
      dapui.setup {
        icons = { expanded = '▾', collapsed = '▸', current_frame = '*' },
        ---@diagnostic disable-next-line: missing-fields
        controls = {
          enabled = true,
          icons = {
            pause = '⏸',
            play = '▶',
            step_into = '⏎',
            step_over = '⏭',
            step_out = '⏮',
            step_back = 'b',
            run_last = '▶▶',
            terminate = '⏹',
            disconnect = '⏏',
          },
        },
      }

      local map = function(keys, func, desc)
        vim.keymap.set('n', keys, func, { desc = 'Debug: ' .. desc })
      end

      map('<leader>Ds', dap.continue, 'Start/continue')
      map('<leader>Di', dap.step_into, 'Step into')
      map('<leader>Do', dap.step_over, 'Step over')
      map('<leader>DO', dap.step_out, 'Step Out')
      map('<leader>Db', dap.toggle_breakpoint, 'Toggle breakpoint')
      map('<leader>Drl', dap.run_last, 'Run last')
      map('<leader>DB', function()
        dap.set_breakpoint(vim.fn.input 'Breakpoint condition: ')
      end, 'Set Breakpoint')
      map('<leader>Drr', dapui.toggle, 'See last session result')
      map('<leader>Dc', dap.run_to_cursor, 'Run to cursor')

      dap.listeners.after.event_initialized['dapui_config'] = dapui.open
      dap.listeners.before.event_terminated['dapui_config'] = dapui.close
      dap.listeners.before.event_exited['dapui_config'] = dapui.close

      -- Language specific debugger setup
      require('dap-go').setup()
    end,
  },

  {
    -- Collection of various small independent plugins/modules
    'echasnovski/mini.nvim',
    version = '*',
    event = { 'InsertEnter', 'VeryLazy' },
    keys = {
      { '<leader>bd',       desc = 'Buffer: delete' },
      { '<leader>sa',       desc = 'Surround: add' },
      { '<leader>sd',       desc = 'Surround: delete' },
      { '<leader>sf',       desc = 'Surround: find right' },
      { '<leader>sF',       desc = 'Surround: Find left' },
      { '<leader>sh',       desc = 'Surround: highlight' },
      { '<leader>sr',       desc = 'Surround: replace' },
      { '<leader>sn',       desc = 'Surround: update n lines' },
      { '\\',               desc = 'Browse files' },
      { '<leader><leader>', desc = 'Find existing buffers' },
      { '<leader>ff',       desc = 'Find files' },
      { '<leader>fg',       desc = 'Find using git' },
      { '<leader>fr',       desc = 'Find resume' },
      { '<leader>fH',       desc = 'Find in help' },
      { '<leader>gf',       desc = 'Grep files' },
      { '<leader>gg',       desc = 'Grep git' },
      { '<leader>Gb',       desc = 'Git branches' },
      { '<leader>Gc',       desc = 'Git commits' },
      { '<leader>fh',       desc = 'Find in command history' },
      { '<leader>fk',       desc = 'Find in key maps' },
      { '<leader>fq',       desc = 'Find in quickfix list' },
      { '<leader>fl',       desc = 'Find in location list' },
      { "<leader>f'",       desc = 'Find in marks' },
      { '<leader>f"',       desc = 'Find in registers' },
      { '<leader>fe',       desc = 'Explore files' },
      { '<leader>fd',       desc = 'Find diagnostics' },
      { '<leader>df',       desc = 'Find diagnostics' },
      { '<leader>bf',       desc = 'Find in buffers' },
      { '<leader>cpd',      desc = 'Code declaration' },
      { '<leader>cpD',      desc = 'Code definition' },
      { '<leader>cps',      desc = 'Code document symbol' },
      { '<leader>cpi',      desc = 'Code implementation' },
      { '<leader>cpr',      desc = 'Code references' },
      { '<leader>cpt',      desc = 'Code type definition' },
      { '<leader>cpw',      desc = 'Code workspace symbols' },
    },
    config = function()
      local function mapn(l, f, desc)
        vim.keymap.set('n', l, f, { desc = desc })
      end

      -- Icons
      require('mini.icons').setup()

      -- Extra features e.g. pickers
      require('mini.extra').setup()

      -- Notifications
      require('mini.notify').setup()

      -- Buffer removal that preserves windows
      require('mini.bufremove').setup()
      mapn('<leader>bd', function() MiniBufremove.delete() end, 'Buffer: delete')

      -- Add/delete/replace surroundings (brackets, quotes, etc.)
      require('mini.surround').setup {
        mappings = {
          add = '<leader>sa',            -- Add surrounding in Normal and Visual modes
          delete = '<leader>sd',         -- Delete surrounding
          find = '<leader>sf',           -- Find surrounding (to the right)
          find_left = '<leader>sF',      -- Find surrounding (to the left)
          highlight = '<leader>sh',      -- Highlight surrounding
          replace = '<leader>sr',        -- Replace surrounding
          update_n_lines = '<leader>sn', -- Update `n_lines`
          suffix_last = 'l',             -- Suffix to search with "prev" method
          suffix_next = 'n',             -- Suffix to search with "next" method
        },
      }

      -- File browser
      require('mini.files').setup {
        mappings = {
          close = '\\'
        },
        windows = {
          preview = true,
        },
      }
      mapn('\\', function() MiniFiles.open() end, 'Browse files')

      -- Picker
      local function minipick_to_quickfix()
        local matches = MiniPick.get_picker_matches()
        if matches == nil then return end
        matches = matches.all
        if matches == nil then return end

        local qfix_contents = {}
        for i = 1, #matches do
          local v = matches[i]
          local nul_start, _ = string.find(v, '\0', 1, true)

          if nul_start == nil then
            table.insert(qfix_contents, {
              filename = v,
              lnum = 1,
              col = 1,
              text = '',
            })
          else
            local parts = vim.split(v, '\0', { plain = true })
            local filename = parts[1]
            local lnum = tonumber(parts[2])
            local col = tonumber(parts[3])
            local text = parts[4]
            if filename ~= nil and lnum ~= nil and col ~= nil and text ~= nil then
              table.insert(qfix_contents, {
                filename = filename,
                lnum = lnum,
                col = col,
                text = text,
              })
            end
          end
        end

        if next(qfix_contents) == nil then
          return
        end
        vim.fn.setqflist(qfix_contents, 'r')
        vim.fn.setqflist({}, 'a', { title = 'MiniPick' })
        MiniPick.stop()
        vim.cmd.copen()
      end

      local find_files_command = {
        'rg',
        '--files',
        '--no-follow',
        '--color=never',
        '--hidden',
        '--iglob',
        '!**/.git/*',
      }

      local show_with_icons = function(buf_id, items, query)
        MiniPick.default_show(buf_id, items, query, { show_icons = true })
      end

      require('mini.pick').setup {
        window = {
          config = function()
            local height = math.floor(0.8 * vim.o.lines)
            if height < 20 then height = vim.o.lines end
            return {
              height = math.floor(0.8 * vim.o.lines),
              width = vim.o.columns,
            }
          end
        },
        mappings = {
          qfixlist = {
            char = '<C-q>',
            func = minipick_to_quickfix,
          },
        },
      }

      mapn('<leader><leader>', function() MiniPick.builtin.buffers() end, 'Find existing buffers')
      mapn('<leader>ff', function() MiniPick.builtin.cli({ command = find_files_command }, { source = { name = 'Files', show = show_with_icons }}) end, 'Find files')
      mapn('<leader>fg', function() MiniPick.builtin.files({ tool = 'git' }) end, 'Find using git')
      mapn('<leader>fr', function() MiniPick.builtin.resume() end, 'Find resume')
      mapn('<leader>fH', function() MiniPick.builtin.help() end, 'Find in help')
      mapn('<leader>gf', function() MiniPick.builtin.grep_live({ tool = 'rg' }) end, 'Grep files')
      mapn('<leader>gg', function() MiniPick.builtin.grep_live({ tool = 'git' }) end, 'Grep git')
      mapn('<leader>Gb', function() MiniExtra.pickers.git_branches() end, 'Git branches')
      mapn('<leader>Gc', function() MiniExtra.pickers.git_commits() end, 'Git commits')
      mapn('<leader>fh', function() MiniExtra.pickers.history() end, 'Find in command history')
      mapn('<leader>fk', function() MiniExtra.pickers.keymaps() end, 'Find in key maps')
      mapn('<leader>fq', function() MiniExtra.pickers.list({ scope = 'quickfix' }) end, 'Find in quickfix list')
      mapn('<leader>fl', function() MiniExtra.pickers.list({ scope = 'location-list' }) end, 'Find in location list')
      mapn("<leader>f'", function() MiniExtra.pickers.marks() end, 'Find in marks')
      mapn('<leader>f"', function() MiniExtra.pickers.registers() end, 'Find in registers')
      mapn('<leader>fe', function() MiniExtra.pickers.explorer() end, 'Explore files')
      mapn('<leader>fd', function() MiniExtra.pickers.diagnostic() end, 'Find diagnostics')
      mapn('<leader>df', function() MiniExtra.pickers.diagnostic() end, 'Find diagnostics')
      mapn('<leader>bf', function() MiniExtra.pickers.buf_lines() end, 'Find in buffers')
      mapn('<leader>cpd', function() MiniExtra.pickers.lsp({ scope = 'declaration' }) end, 'Code declaration')
      mapn('<leader>cpD', function() MiniExtra.pickers.lsp({ scope = 'definition' }) end, 'Code definition')
      mapn('<leader>cps', function() MiniExtra.pickers.lsp({ scope = 'document_symbol' }) end, 'Code document symbol')
      mapn('<leader>cpi', function() MiniExtra.pickers.lsp({ scope = 'implementation' }) end, 'Code implementation')
      mapn('<leader>cpr', function() MiniExtra.pickers.lsp({ scope = 'references' }) end, 'Code references')
      mapn('<leader>cpt', function() MiniExtra.pickers.lsp({ scope = 'type_definition' }) end, 'Code type definition')
      mapn('<leader>cpw', function() MiniExtra.pickers.lsp({ scope = 'workspace_symbol' }) end, 'Code workspace symbols')

      -- Clues
      local miniclue = require('mini.clue')
      miniclue.setup {
        triggers = {
          -- Leader triggers
          { mode = 'n', keys = '<Leader>' },
          { mode = 'x', keys = '<Leader>' },

          -- Built-in completion
          { mode = 'i', keys = '<C-x>' },

          -- `g` key
          { mode = 'n', keys = 'g' },
          { mode = 'x', keys = 'g' },

          -- Marks
          { mode = 'n', keys = "'" },
          { mode = 'n', keys = '`' },
          { mode = 'x', keys = "'" },
          { mode = 'x', keys = '`' },

          -- Registers
          { mode = 'n', keys = '"' },
          { mode = 'x', keys = '"' },
          { mode = 'i', keys = '<C-r>' },
          { mode = 'c', keys = '<C-r>' },

          -- Window commands
          { mode = 'n', keys = '<C-w>' },

          -- `z` key
          { mode = 'n', keys = 'z' },
          { mode = 'x', keys = 'z' },
        },
        clues = {
          { mode = 'n', keys = '<Leader>b', desc = '+Buffer' },
          { mode = 'n', keys = '<Leader>c', desc = '+Code' },
          { mode = 'n', keys = '<Leader>d', desc = '+Diagnostic' },
          { mode = 'n', keys = '<Leader>D', desc = '+Debug' },
          { mode = 'n', keys = '<Leader>f', desc = '+Find' },
          { mode = 'n', keys = '<Leader>g', desc = '+Grep' },
          { mode = 'n', keys = '<Leader>G', desc = '+Git' },
          { mode = 'n', keys = '<Leader>h', desc = '+Harpoon' },
          { mode = 'n', keys = '<Leader>s', desc = '+Surround' },
          { mode = 'n', keys = '<Leader>t', desc = '+Toggle' },
          { mode = 'n', keys = '<Leader>W', desc = '+Wiki' },
          miniclue.gen_clues.builtin_completion(),
          miniclue.gen_clues.g(),
          miniclue.gen_clues.marks(),
          miniclue.gen_clues.registers(),
          miniclue.gen_clues.windows(),
          miniclue.gen_clues.z(),
        },
        window = {
          config = {
            width = 'auto',
          },
          delay = 400,
        },
      }
    end,
  },

  {
    -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    event = { 'BufReadPost', 'BufNewFile' },
    cmd = { 'TSInstall', 'TSUpdate' },
    build = ':TSUpdate',
    dependencies = {
      -- Selection expansion
      'RRethy/nvim-treesitter-textsubjects',
    },
    opts = {
      ensure_installed = { 'git_rebase' },
      auto_install = true,
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
      },
      indent = { enable = true, disable = { 'c', 'cpp', 'yaml' } },
      textsubjects = {
        enable = true,
        prev_selection = ',',
        keymaps = {
          ['.'] = { 'textsubjects-smart', desc = 'Expand selection' },
          [';'] = { 'textsubjects-container-outer', desc = 'Select outside containers (classes, functions, etc.)' },
          ['i;'] = { 'textsubjects-container-inner', desc = 'Select inside containers (classes, functions, etc.)' },
        },
      },
    },
    config = function(_, opts)
      require('nvim-treesitter.install').prefer_git = true
      ---@diagnostic disable-next-line: missing-fields
      require('nvim-treesitter.configs').setup(opts)
    end,
  },

  {
    -- Undo UI
    'mbbill/undotree',
    cmd = 'UndotreeToggle',
    keys = {
      { '<leader>u', '<cmd>UndotreeToggle<cr>', desc = 'Toggle Undotree' },
    },
  },

  {
    -- Rearrange windows
    'sindrets/winshift.nvim',
    cmd = 'WinShift',
    keys = {
      { '<leader>zz', '<cmd>WinShift<cr>', desc = 'WinShift' },
    },
  },

  {
    -- Wiki
    'vimwiki/vimwiki',
    cmd = { 'VimwikiIndex', 'VimwikiMakeDiaryNote' },
    event = 'BufEnter *.md',
    keys = {
      { '<leader>Ww', desc = 'Wiki: Open default index file' },
      { '<leader>Ws', desc = 'Wiki: Select and open index file' },
      { '<leader>Wd', desc = 'Wiki: Create diary note' },
    },
    init = function()
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
    end
  },

  {
    -- Lua LSP
    'folke/lazydev.nvim',
    ft = 'lua',
    opts = {
      library = {
        -- Load luvit types when the `vim.uv` word is found
        { path = '${3rd}/luv/library', words = { 'vim%.uv' } },
      },
    },
  },

  {
    -- Zig
    'ziglang/zig.vim',
    ft = { 'zig' },
  },

  {
    -- Scala
    'scalameta/nvim-metals',
    ft = { 'scala', 'sbt', 'java' },
    dependencies = {
      'nvim-lua/plenary.nvim',
    },
    opts = function()
      local metals_config = require('metals').bare_config()
      metals_config.on_attach = function()
        require('metals').setup_dap()
      end
      metals_config.settings = {
        autoImportBuild = 'off',
      }

      return metals_config
    end,
    config = function(self, metals_config)
      local nvim_metals_group = vim.api.nvim_create_augroup('nvim-metals', { clear = true })
      vim.api.nvim_create_autocmd('FileType', {
        pattern = self.ft,
        callback = function()
          require('metals').initialize_or_attach(metals_config)
        end,
        group = nvim_metals_group,
      })
    end,
  },

  {
    -- Pkl
    'apple/pkl-neovim',
    ft = 'pkl',
    dependencies = {
      'nvim-treesitter/nvim-treesitter',
    },
    build = function()
      require('pkl-neovim').init()
    end,
    config = function()
      vim.g.pkl_neovim = {
        start_command = { 'pkl-lsp' },
      }
    end,
  },
}

-- Load plugins
require('lazy').setup(lazy_plugins, {
  ui = {
    icons = vim.g.have_nerd_font and {} or {
      cmd = '⌘',
      config = '🛠',
      event = '📅',
      ft = '📂',
      init = '⚙',
      keys = '🗝',
      plugin = '🔌',
      runtime = '💻',
      require = '🌙',
      source = '📄',
      start = '🚀',
      task = '📌',
      lazy = '💤 ',
    },
  },
})

-- vim: ts=2 sts=2 sw=2 et
