--
-- Appearance
--
vim.g.have_nerd_font = true
vim.opt.number = false
vim.opt.relativenumber = false
vim.opt.showmode = true
vim.opt.laststatus = 0
vim.opt.breakindent = true
vim.opt.signcolumn = 'yes'
vim.opt.list = true
vim.opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }
vim.opt.cursorline = false
vim.opt.wrap = true
vim.opt.termguicolors = true
vim.opt.shortmess:append 'I'
vim.opt.title = false

--
-- UI
--
vim.opt.updatetime = 250
vim.opt.timeoutlen = 300 -- Faster popup
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.inccommand = 'split' -- preview subtitutions
vim.opt.scrolloff = 10       -- min screen lines
vim.opt.errorbells = false
vim.opt.ignorecase = true
vim.opt.wildignorecase = true
vim.opt.wildmode = 'full:lastused'
vim.opt.foldcolumn = '0'
vim.opt.foldtext = ''
vim.opt.foldnestmax = 3
vim.opt.foldlevelstart = 99
vim.opt.foldmethod = 'indent'
vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 3
vim.g.netrw_altfile = 1
vim.cmd.hi 'Normal guibg=NONE guifg=NONE ctermbg=NONE ctermfg=NONE'

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

--
-- Search
--
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = false
vim.opt.gdefault = true
vim.opt.infercase = true
vim.opt.incsearch = true
if vim.fn.executable 'rg' == 1 then
  vim.opt.grepprg = 'rg --vimgrep --no-heading --smart-case'
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
  end, { desc = 'Copy full path to clipboard' })
  cmd('CopyRelPath', function()
    local path = vim.fn.expand('%:.')
    vim.fn.setreg('+', path)
  end, { desc = 'Copy relative path to clipboard' })
end

--
-- Autocommands
--  See `:help lua-guide-autocommands`
--
do
  local autocmd = vim.api.nvim_create_autocmd
  -- Highlight when yanking (copying) text
  autocmd('TextYankPost', {
    desc = 'Highlight when yanking (copying) text',
    group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
    callback = function()
      vim.highlight.on_yank()
    end,
  })
  -- Autosave
  autocmd({ 'CursorHold', 'TextChanged', 'InsertLeave' }, {
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

  -- Diagnostic keymaps
  mapk('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous [d]iagnostic message' })
  mapk('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next [d]iagnostic message' })
  mapk('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Show diagnostic [e]rror messages' })
  mapk('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [q]uickfix list' })

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
  mapk('i', '<M-Backspace>', '<C-w>', { desc = 'Delete word' })
  mapk('c', '<M-Backspace>', '<C-w>', { desc = 'Delete word' })
  mapk('v', '<leader>c', '"+y', { desc = 'Copy to system clipboard' })
  mapk('n', '<leader>v', '"+p', { desc = 'Paste from system clipboard' })
  mapk('n', 'x', '"_dl', { noremap = true, desc = 'Delete character' })
  mapk('n', 'X', '"_dh', { noremap = true, desc = 'Backspace' })
  mapk('n', '<leader>x', '"_d', { noremap = true, desc = 'Delete (w/o yank)' })
  mapk('n', '<leader>X', '"_D', { noremap = true, desc = 'Delete to EOL (w/o yank)' })
  mapk('n', 'Y', 'y$', { noremap = true, desc = 'Yank the rest of the line' })

  -- Keep selection on indentation
  mapk('v', '<', '<gv', { desc = 'Lower indent and keep selection' })
  mapk('v', '>', '>gv', { desc = 'Increase indent and keep selection' })

  -- Move selected lines up/down
  mapk('v', 'J', ":m '>+1<CR>gv=gv", { desc = 'Move lines down' })
  mapk('v', 'K', ":m '<-2<CR>gv=gv", { desc = 'Move lines up' })

  -- Files and buffers
  mapk('n', '<leader>oe', ":e <C-R>=expand('%:p:h') . '/' <cr>", { desc = '[O]pen file from current buffer directory' })
  mapk('n', '<leader>J', '<cmd>lnext<cr>zz', { desc = 'Location list next' })
  mapk('n', '<leader>K', '<cmd>lprev<cr>zz', { desc = 'Location list previous' })
  mapk('n', '<leader>j', '<cmd>cnext<cr>zz', { desc = 'Quickfix next' })
  mapk('n', '<leader>k', '<cmd>cprev<cr>zz', { desc = 'Quickfix previous' })
  mapk('n', '\\', '<cmd>Ex<cr>', { desc = 'File explorer' })

  -- Command mode navigation
  mapk('c', '<C-f>', '<right>', { desc = 'Move cursor right' })
  mapk('c', '<C-b>', '<left>', { desc = 'Move cursor left' })
  mapk('c', '<M-f>', '<c-right>', { desc = 'Move cursor one word right' })
  mapk('c', '<M-b>', '<c-left>', { desc = 'Move cursor one word left' })
  mapk('c', '<M-right>', '<c-right>', { desc = 'Move cursor one word right' })
  mapk('c', '<M-left>', '<c-left>', { desc = 'Move cursor one word left' })

  -- Toggles
  mapk('n', '<leader>th', '<cmd>setlocal hlsearch!<cr>', { desc = '[T]oggle search [h]ilight' })
  mapk('n', '<leader>tp', '<cmd>set paste!<cr>', { desc = '[T]oggle [p]aste mode' })
  mapk('n', '<leader>tn', '<cmd>set number!<cr>', { desc = '[T]oggle [n]umbers' })
  mapk('n', '<leader>tr', '<cmd>set relativenumber!<cr>', { desc = '[T]oggle [r]elative numbers' })
  mapk('n', '<leader>tw', '<cmd>setlocal wrap!<cr>', { desc = '[T]oggle line [w]rapping' })
  mapk('n', '<leader>tcl', '<cmd>setlocal cursorline!<cr>', { desc = '[T]oggle [c]ursor line' })
  mapk('n', '<leader>tcc', '<cmd>setlocal cursorcolumn!<cr>', { desc = '[T]oggle [c]ursor line' })
  mapk('n', '<leader>tas', '<cmd>ToggleAutoSave<cr>', { desc = '[T]oggle [a]uto[s]ave' })
end

--
-- Plugin manager
--    See `:help lazy.nvim.txt` or https://github.com/folke/lazy.nvim for more info
--

-- Plugin setup
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
  vim.fn.system { 'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo, lazypath }
end ---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)

--
-- Configure and install plugins
--
require('lazy').setup({
  {
    -- Async make
    'tpope/vim-dispatch',
    cmd = { 'Make', 'Dispatch', 'Start' },
  },

  {
    -- Detect tabstop and shiftwidth automatically
    'tpope/vim-sleuth',
    event = { 'BufReadPost', 'BufNewFile' },
  },

  {
    'numToStr/Comment.nvim',
    keys = {
      { mode = 'n', '<C-_>', desc = 'Toggle comment on current line' },
      { mode = 'n', '<C-/>', desc = 'Toggle comment on current line' },
      { mode = 'x', '<C-_>', desc = 'Toggle comment on current selection' },
      { mode = 'x', '<C-/>', desc = 'Toggle comment on current selection' },
    },
    config = function()
      local comment = require 'Comment'
      local api = require 'Comment.api'
      comment.setup()

      local comment_keys = { '<C-_>', '<C-/>' }
      for _, binding in pairs(comment_keys) do
        vim.keymap.set('n', binding, api.locked 'toggle.linewise.current', { desc = 'Toggle comment on current line' })

        local esc = vim.api.nvim_replace_termcodes('<ESC>', true, false, true)
        vim.keymap.set('x', binding, function()
          vim.api.nvim_feedkeys(esc, 'nx', false)
          api.toggle.linewise(vim.fn.visualmode())
        end, { desc = 'Toggle comment on current selection' })
      end
    end,
  },

  {
    -- Adds git related signs to the gutter, as well as utilities for managing changes
    'lewis6991/gitsigns.nvim',
    cmd = { 'Gitsigns' },
    keys = {
      { '[gc',         desc = 'Jump to previous [g]it [c]hange' },
      { ']gc',         desc = 'Jump to next [g]it [c]hange' },
      { '<leader>tgs', desc = '[T]oggle [g]it [s]igns' },
      { '<leader>tgb', desc = '[T]oggle [g]it [b]lame line' },
      { '<leader>tgd', desc = '[T]oggle [g]it [d]eleted lines' },
    },
    opts = {
      on_attach = function(bufnr)
        local gitsigns = require 'gitsigns'

        local function map(mode, l, r, opts)
          opts = opts or {}
          opts.buffer = bufnr
          vim.keymap.set(mode, l, r, opts)
        end

        -- Navigation
        map('n', '[gc', function()
          if vim.wo.diff then
            vim.cmd.normal { '[gc', bang = true }
          else
            gitsigns.nav_hunk 'prev'
          end
        end, { desc = 'Jump to previous [g]it [c]hange' })
        map('n', ']gc', function()
          if vim.wo.diff then
            vim.cmd.normal { ']gc', bang = true }
          else
            gitsigns.nav_hunk 'next'
          end
        end, { desc = 'Jump to next [g]it [c]hange' })

        -- Toggles
        map('n', '<leader>tgs', gitsigns.toggle_signs, { desc = '[T]oggle [g]it [s]igns' })
        map('n', '<leader>tgb', gitsigns.toggle_current_line_blame, { desc = '[T]oggle [g]it [b]lame line' })
        map('n', '<leader>tgd', gitsigns.toggle_deleted, { desc = '[T]oggle [g]it [d]eleted lines' })
      end,
    },
  },

  {
    -- Git UI
    'tpope/vim-fugitive',
    cmd = { 'Git', 'Gedit', 'Ge' },
  },

  {
    -- Show you pending keybinds.
    'folke/which-key.nvim',
    event = 'VeryLazy',
    keys = {
      {
        '<leader>?',
        function()
          require('which-key').show({ global = false })
        end,
        desc = 'Buffer Local Keymaps (which-key)',
      },
    },
    config = function()
      local whichKey = require 'which-key'
      whichKey.setup()

      -- Document existing key chains
      whichKey.add {
        { '<leader>b', group = '[B]uffer', },
        { '<leader>c', group = '[C]ode', },
        { '<leader>d', group = '[D]ebug', },
        { '<leader>f', group = '[F]ind', },
        { '<leader>h', group = '[H]arpoon', },
        { '<leader>s', group = '[S]urround', },
        { '<leader>t', group = '[T]oggle', },
      }
    end,
  },

  {
    -- Fuzzy Finder (files, lsp, etc)
    'nvim-telescope/telescope.nvim',
    cmd = 'Telescope',
    keys = {
      { '<leader>fh',       desc = '[F]ind [h]elp' },
      { '<leader>fk',       desc = '[F]ind [k]eymaps' },
      { '<leader>ff',       desc = '[F]ind [f]iles' },
      { '<leader>fp',       desc = '[F]ind Git files' },
      { '<leader>fs',       desc = '[F]ind [s]elect Telescope' },
      { '<leader>fw',       desc = '[F]ind current [w]ord' },
      { '<leader>fg',       desc = '[F]ind by [g]rep' },
      { '<leader>fd',       desc = '[F]ind [d]iagnostics' },
      { '<leader>fr',       desc = '[F]ind [r]esume' },
      { '<leader>f.',       desc = '[F]ind Recent Files ("." for repeat,' },
      { '<leader>fq',       desc = '[F]ind [q]uickfix' },
      { '<leader><leader>', desc = '[ ] Find existing buffers' },
      { '<leader>/',        desc = '[/] Fuzzily search in current buffer' },
      { '<leader>f/',       desc = '[F]ind [/] in Open Files' },
      { '<leader>fn',       desc = '[F]ind [n]eovim files' },
    },
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        build = 'make',
        cond = function()
          return vim.fn.executable 'make' == 1
        end,
      },
      { 'nvim-telescope/telescope-ui-select.nvim' },
      {
        'nvim-tree/nvim-web-devicons',
        enabled = vim.g.have_nerd_font
      },
    },
    config = function()
      require('telescope').setup {
        extensions = {
          ['ui-select'] = {
            require('telescope.themes').get_dropdown(),
          },
        },
        pickers = {
          find_files = {
            find_command = {
              'rg',
              '--files',
              '--hidden',
              '--glob',
              '!**/.git/*',
            }
          },
        },
        defaults = {
          path_display = {
            'truncate',
          },
          vimgrep_arguments = {
            'rg',
            '--hidden',
            '--color=never',
            '--no-heading',
            '--with-filename',
            '--line-number',
            '--column',
            '--smart-case',
            '--trim',
            '--iglob',
            '!**/.git/*',
          },
          layout_config = {
            width = { padding = 0, },
            height = { padding = 0 },
            preview_cutoff = 120,
          },
        },
      }

      -- Enable Telescope extensions if they are installed
      pcall(require('telescope').load_extension, 'fzf')
      pcall(require('telescope').load_extension, 'ui-select')

      local builtin = require 'telescope.builtin'
      vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = '[F]ind [h]elp' })
      vim.keymap.set('n', '<leader>fk', builtin.keymaps, { desc = '[F]ind [k]eymaps' })
      vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = '[F]ind [f]iles' })
      vim.keymap.set('n', '<leader>fp', builtin.git_files, { desc = '[F]ind Git files' })
      vim.keymap.set('n', '<leader>fs', builtin.builtin, { desc = '[F]ind [s]elect Telescope' })
      vim.keymap.set('n', '<leader>fw', builtin.grep_string, { desc = '[F]ind current [w]ord' })
      vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = '[F]ind by [g]rep' })
      vim.keymap.set('n', '<leader>fd', builtin.diagnostics, { desc = '[F]ind [d]iagnostics' })
      vim.keymap.set('n', '<leader>fr', builtin.resume, { desc = '[F]ind [r]esume' })
      vim.keymap.set('n', '<leader>f.', builtin.oldfiles, { desc = '[F]ind Recent Files ("." for repeat)' })
      vim.keymap.set('n', '<leader>fq', builtin.quickfix, { desc = '[F]ind [q]uickfix' })
      vim.keymap.set('n', '<leader><leader>', builtin.buffers, { desc = '[ ] Find existing buffers' })

      vim.keymap.set('n', '<leader>/', function()
        builtin.current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
          winblend = 10,
          previewer = false,
        })
      end, { desc = '[/] Fuzzily search in current buffer' })

      vim.keymap.set('n', '<leader>f/', function()
        builtin.live_grep {
          grep_open_files = true,
          prompt_title = 'Live Grep in Open Files',
        }
      end, { desc = '[F]ind [/] in Open Files' })

      vim.keymap.set('n', '<leader>fn', function()
        builtin.find_files { cwd = vim.fn.stdpath 'config' }
      end, { desc = '[F]ind [n]eovim files' })
    end,
  },

  {
    -- Quick jump between files
    'ThePrimeagen/harpoon',
    branch = 'harpoon2',
    keys = {
      { '<leader>hh', desc = '[H]arpoon list in Telescope' },
      { '<leader>hq', desc = '[H]arpoon [q]uick menu' },
      { '<leader>ha', desc = '[H]arpoon [a]dd buffer' },
      { '<leader>hx', desc = '[H]arpoon remove buffer' },
      { '<leader>1', desc = '[H]arpoon buffer [1]' },
      { '<leader>2', desc = '[H]arpoon buffer [2]' },
      { '<leader>3', desc = '[H]arpoon buffer [3]' },
      { '<leader>4', desc = '[H]arpoon buffer [4]' },
      { '[h', desc = '[H]arpoon previous buffer' },
      { ']h', desc = '[H]arpoon next buffer' },
    },
    requires = { { 'nvim-lua/plenary.nvim' } },
    config = function()
      local harpoon = require 'harpoon'
      harpoon:setup()

      local conf = require('telescope.config').values
      local function toggle_telescope(harpoon_files)
        local file_paths = {}
        for _, item in ipairs(harpoon_files.items) do
          table.insert(file_paths, item.value)
        end

        require('telescope.pickers')
          .new({}, {
            prompt_title = 'Harpoon',
            finder = require('telescope.finders').new_table {
              results = file_paths,
            },
            previewer = conf.file_previewer {},
            sorter = conf.generic_sorter {},
          })
          :find()
      end

      local mapk = vim.keymap.set
      mapk('n', '<leader>hh', function()
        toggle_telescope(harpoon:list())
      end, { desc = '[H]arpoon list in Telescope' })
      mapk('n', '<leader>hq', function()
        harpoon.ui:toggle_quick_menu(harpoon:list())
      end, { desc = '[H]arpoon [q]uick menu' })
      mapk('n', '<leader>ha', function()
        harpoon:list():add()
      end, { desc = '[H]arpoon [a]dd buffer' })
      mapk('n', '<leader>hx', function()
        harpoon:list():remove()
      end, { desc = '[H]arpoon remove buffer' })
      mapk('n', '<leader>1', function()
        harpoon:list():select(1)
      end, { desc = '[H]arpoon buffer [1]' })
      mapk('n', '<leader>2', function()
        harpoon:list():select(2)
      end, { desc = '[H]arpoon buffer [2]' })
      mapk('n', '<leader>3', function()
        harpoon:list():select(3)
      end, { desc = '[H]arpoon buffer [3]' })
      mapk('n', '<leader>4', function()
        harpoon:list():select(4)
      end, { desc = '[H]arpoon buffer [4]' })
      mapk('n', '[h', function()
        harpoon:list():prev { ui_nav_wrap = true }
      end, { desc = '[H]arpoon [p]revious buffer' })
      mapk('n', ']h', function()
        harpoon:list():next { ui_nav_wrap = true }
      end, { desc = '[H]arpoon [n]ext buffer' })
    end,
  },

  {
    -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    cmd = { 'LspInfo', 'LspInstall', 'LspUninstall', 'LspStart', },
    dependencies = {
      -- Automatically install LSPs and related tools to stdpath for Neovim
      { 'williamboman/mason.nvim', config = true },
      'williamboman/mason-lspconfig.nvim',

      -- Useful status updates for LSP.
      { 'j-hui/fidget.nvim',       opts = {} },

      -- `neodev` configures Lua LSP for your Neovim config, runtime and plugins
      -- used for completion, annotations and signatures of Neovim apis
      { 'folke/neodev.nvim',       opts = {} },
    },
    config = function()
      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('kickstart-lsp-attach', { clear = true }),
        callback = function(event)
          vim.g.lsp_doc_hl_enabled = false
          local telescope = require('telescope.builtin')
          local map = function(keys, func, desc)
            vim.keymap.set('n', keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
          end

          map('K', vim.lsp.buf.hover, 'Hover Documentation')
          map('gd', telescope.lsp_definitions, '[G]oto [d]efinition') -- To jump back, press <C-t>.
          map('gr', telescope.lsp_references, '[G]oto [r]eferences')
          map('gI', telescope.lsp_implementations, '[G]oto [i]mplementation')
          map('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
          map('<leader>Dd', telescope.lsp_type_definitions, 'Type [D]efinition')
          map('<leader>Ds', telescope.lsp_document_symbols, '[D]ocument [s]ymbols')
          map('<leader>ca', vim.lsp.buf.code_action, '[C]ode [a]ction')
          map('<leader>cf', vim.lsp.buf.format, '[C]ode [f]ormat')
          map('<leader>cl', vim.lsp.codelens.run, '[C]ode [l]ens')
          map('<leader>cs', vim.lsp.buf.signature_help, '[C]ode [s]ignature')
          map('<leader>cr', vim.lsp.buf.rename, '[C]ode [R]ename')
          map('<leader>ws', telescope.lsp_dynamic_workspace_symbols, '[W]orkspace [s]ymbols')
          map('<leader>we', function()
            vim.diagnostic.setqflist { severity = vim.diagnostic.severity.ERROR }
          end, '[W]orkspace [e]rrors')
          map('<leader>ww', function()
            vim.diagnostic.setqflist { severity = vim.diagnostic.severity.WARN }
          end, '[W]orkspace [w]arnings')
          map('<leader>tdh', function()
            vim.g.lsp_doc_hl_enabled = not vim.g.lsp_doc_hl_enabled
          end, '[T]oggle [d]ocument [h]ighlight')

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

          -- Inlay hints
          if client and client.server_capabilities.inlayHintProvider and vim.lsp.inlay_hint then
            map('<leader>tih', function()
              vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
            end, '[T]oggle [i]nlay [h]ints')
          end
        end,
      })

      -- Look and feel
      local border = {
        { '🭽', 'FloatBorder' },
        { '▔', 'FloatBorder' },
        { '🭾', 'FloatBorder' },
        { '▕', 'FloatBorder' },
        { '🭿', 'FloatBorder' },
        { '▁', 'FloatBorder' },
        { '🭼', 'FloatBorder' },
        { '▏', 'FloatBorder' },
      }
      local handlers = {
        ['textDocument/hover'] = vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
        ['textDocument/signatureHelp'] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = border }),
      }
      vim.diagnostic.config({
        virtual_text = {
          prefix = '■ ',
        },
        update_in_insert = false,
        float = { border = border },
      })

      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = vim.tbl_deep_extend('force', capabilities, require('cmp_nvim_lsp').default_capabilities())

      -- Enable the following language servers
      -- See `:help lspconfig-all` for a list of all the pre-configured LSPs
      local servers = {
        ts_ls = {
          init_options = {
            preferences = {
              disableSuggestions = true,
            },
          },
        },
        clangd = {},
        bashls = {},
        zls = {},
        ltex = {
          autostart = false,
          settings = {
            language = 'en-GB',
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
            },
          },
        },
      }

      -- Ensure the servers and tools above are installed
      require('mason').setup()

      local lspconfig = require('lspconfig')
      require('mason-lspconfig').setup {
        ensure_installed = {},
        automatic_installation = false,
        handlers = {
          function(server_name)
            local server = servers[server_name] or {}
            server.capabilities = vim.tbl_deep_extend('force', {}, capabilities, server.capabilities or {})
            server.handlers = vim.tbl_deep_extend('force', {}, handlers, server.handlers or {})
            lspconfig[server_name].setup(server)
          end,
        },
      }
    end,
  },

  {
    -- Debug Adapter Protocol (DAP)
    'mfussenegger/nvim-dap',
    keys = {
      { '<leader>ds',  desc = '[S]tart/continue' },
      { '<leader>di',  desc = 'Step [i]nto' },
      { '<leader>do',  desc = 'Step [o]ver' },
      { '<leader>dO',  desc = 'Step [O]ut' },
      { '<leader>db',  desc = 'Toggle [b]reakpoint' },
      { '<leader>drl', desc = '[R]un [l]ast' },
      { '<leader>dB',  desc = 'Set [B]reakpoint' },
      { '<leader>drr', desc = 'See last session [r]esult' },
      { '<leader>dc',  desc = 'Run to [c]ursor' },
    },
    dependencies = {
      'rcarriga/nvim-dap-ui',
      'nvim-neotest/nvim-nio',
      'williamboman/mason.nvim',
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

      map('<leader>ds', dap.continue, '[S]tart/continue')
      map('<leader>di', dap.step_into, 'Step [i]nto')
      map('<leader>do', dap.step_over, 'Step [o]ver')
      map('<leader>dO', dap.step_out, 'Step [O]ut')
      map('<leader>db', dap.toggle_breakpoint, 'Toggle [b]reakpoint')
      map('<leader>drl', dap.run_last, '[R]un [l]ast')
      map('<leader>dB', function()
        dap.set_breakpoint(vim.fn.input 'Breakpoint condition: ')
      end, 'Set [B]reakpoint')
      map('<leader>drr', dapui.toggle, 'See last session [r]esult')
      map('<leader>dc', dap.run_to_cursor, 'Run to [c]ursor')

      dap.listeners.after.event_initialized['dapui_config'] = dapui.open
      dap.listeners.before.event_terminated['dapui_config'] = dapui.close
      dap.listeners.before.event_exited['dapui_config'] = dapui.close

      -- Language specific debugger setup
      require('dap-go').setup()
    end,
  },

  {
    -- Autoformat
    'stevearc/conform.nvim',
    keys = {
      {
        '<leader>cf',
        function()
          require('conform').format { async = true, lsp_fallback = true }
        end,
        mode = '',
        desc = '[C]ode [f]ormat',
      },
    },
    opts = {
      notify_on_error = false,
      formatters_by_ft = {
        lua = { 'stylua' },
        python = { 'black' },
        javascript = { { 'prettierd', 'prettier' } },
        typescript = { { 'prettierd', 'prettier' } },
        javascriptreact = { { 'prettierd', 'prettier' } },
        typescriptreact = { { 'prettierd', 'prettier' } },
        ['_'] = { 'trim_whitespace' },
      },
    },
  },

  {
    -- Auto-completion
    'hrsh7th/nvim-cmp',
    event = 'InsertEnter',
    dependencies = {
      -- Snippet Engine & its associated nvim-cmp source
      {
        'L3MON4D3/LuaSnip',
        build = (function()
          -- Build Step is needed for regex support in snippets.
          -- This step is not supported in many windows environments.
          -- Remove the below condition to re-enable on windows.
          if vim.fn.has 'win32' == 1 or vim.fn.executable 'make' == 0 then
            return
          end
          return 'make install_jsregexp'
        end)(),
        dependencies = {
          'rafamadriz/friendly-snippets',
        },
      },
      'saadparwaiz1/cmp_luasnip',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',
    },
    config = function()
      -- See `:help cmp`
      local cmp = require 'cmp'
      local luasnip = require 'luasnip'
      luasnip.config.setup {}

      cmp.setup {
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        completion = {
          completeopt = 'menu,menuone,noinsert',
          autocomplete = false,
        },
        window = {
          completion = cmp.config.window.bordered(),
          documentation = cmp.config.window.bordered(),
        },

        -- Please read `:help ins-completion`
        mapping = cmp.mapping.preset.insert {
          ['<C-n>'] = cmp.mapping.select_next_item(),
          ['<C-p>'] = cmp.mapping.select_prev_item(),
          ['<C-b>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-y>'] = cmp.mapping.confirm { select = true },
          ['<C-Space>'] = cmp.mapping.complete {},

          -- Think of <c-l> as moving to the right of your snippet expansion.
          --  So if you have a snippet that's like:
          --  function $name($args)
          --    $body
          --  end
          --
          -- <c-l> will move you to the right of each of the expansion locations.
          -- <c-h> is similar, except moving you backwards.
          ['<C-l>'] = cmp.mapping(function()
            if luasnip.expand_or_locally_jumpable() then
              luasnip.expand_or_jump()
            end
          end, { 'i', 's' }),
          ['<C-h>'] = cmp.mapping(function()
            if luasnip.locally_jumpable(-1) then
              luasnip.jump(-1)
            end
          end, { 'i', 's' }),
        },
        sources = {
          { name = 'nvim_lsp' },
          { name = 'luasnip' },
          { name = 'path' },
        },
      }
    end,
  },

  {
    -- Highlight todo, notes, etc in comments
    'folke/todo-comments.nvim',
    cmd = { 'TodoTelescope', 'TodoQuickFix', 'TodoLocList', },
    dependencies = { 'nvim-lua/plenary.nvim' },
    opts = { signs = false },
  },

  {
    -- Collection of various small independent plugins/modules
    'echasnovski/mini.nvim',
    keys = {
      { '<leader>bd', desc = '[B]uffer: [D]elete' },
      { '<leader>sa', desc = '[S]urround: [a]dd' },
      { '<leader>sd', desc = '[S]urround: [d]elete' },
      { '<leader>sf', desc = '[S]urround: [f]ind right' },
      { '<leader>sF', desc = '[S]urround: [F]ind left' },
      { '<leader>sh', desc = '[S]urround: [h]ighlight' },
      { '<leader>sr', desc = '[S]urround: [r]eplace' },
      { '<leader>sn', desc = '[S]urround: update [n] lines' },
    },
    config = function()
      -- Buffer removal that preserves windows
      require('mini.bufremove').setup()
      vim.keymap.set('n', '<leader>bd', function()
        MiniBufremove.delete()
      end, { desc = '[B]uffer: [D]elete' })

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
    end,
  },

  {
    -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    event = { 'BufReadPost', 'BufNewFile' },
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
    -- Automatic parenthesis etc.
    'windwp/nvim-autopairs',
    event = 'InsertEnter',
    dependencies = { 'hrsh7th/nvim-cmp' },
    config = function()
      require('nvim-autopairs').setup {}
      -- If you want to automatically add `(` after selecting a function or method
      local cmp_autopairs = require 'nvim-autopairs.completion.cmp'
      local cmp = require 'cmp'
      cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
    end,
  },

  {
    -- Templates
    'glepnir/template.nvim',
    cmd = { 'Template', 'TemProject' },
    config = function()
      require('template').setup({
        temp_dir = '~/.config/nvim/templates',
        author = 'Jaakko Pallari',
      })
    end
  },

  {
    -- File manager
    'nvim-neo-tree/neo-tree.nvim',
    version = '*',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-tree/nvim-web-devicons',
      'MunifTanjim/nui.nvim',
    },
    cmd = 'Neotree',
    keys = {
      { '\\', ':Neotree float reveal<CR>', { desc = 'NeoTree reveal' } },
    },
    opts = {
      filesystem = {
        filtered_items = {
          visible = true,
          hide_dotfiles = false,
          hide_gitignored = true,
          hide_by_name = {
            '.git',
            '.DS_Store',
            'thumbs.db',
          },
          never_show = {},
        },
        group_empty_dirs = true,
        window = {
          position = 'float',
          mappings = {
            ['\\'] = 'close_window',
          },
          popup = {
            size = { height = '100%', width = '100%', },
          },
        },
      },
    },
  },

  {
    -- Rust
    'mrcjkb/rustaceanvim',
    version = '^4',
    ft = { 'rust' },
    dependencies = {
      'hrsh7th/nvim-cmp',
      'hrsh7th/cmp-nvim-lsp',
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
    dependencies = {
      'nvim-lua/plenary.nvim',
    },
    ft = { 'scala', 'sbt', 'java' },
    opts = function()
      local metals_config = require('metals').bare_config()
      metals_config.on_attach = function()
        require('metals').setup_dap()
      end
      metals_config.autoImportBuild = 'off'

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
    -- GitHub Copilot
    'github/copilot.vim',
    cmd = { 'Copilot' },
  },

  {
    -- Wiki
    'vimwiki/vimwiki',
    cmd = { 'VimwikiIndex', 'VimwikiMakeDiaryNote' },
    event = 'BufEnter *.md',
    keys = {
      { '<leader>ww', desc = '[W]iki: Open default [w]iki index file' }, 
      { '<leader>ws', desc = '[W]iki: [S]elect and open wiki index file' }, 
      { '<leader>w<leader>w', desc = '[W]iki: Create diary note' }, 
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
  }
}, {
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
