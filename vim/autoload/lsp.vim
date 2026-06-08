vim9script

const SID = expand('<SID>')

# ---------------------------------------------------------------------------
# Plugin options
# ---------------------------------------------------------------------------

const LSP_OPTIONS = {
  ignoreMissingServer: true,
  autoHighlightDiags: true,
  highlightDiagInline: false,
  showDiagWithVirtualText: false,
  showDiagInPopup: true,
  autoHighlight: false,
  showInlayHints: false,
  hoverFallback: true,
  definitionFallback: true,
  autoComplete: false,
  omniComplete: true,
  customCompletionKinds: true,
  completionKinds: {
    Text: 'Txt',
    Method: 'Mtd',
    Function: 'Fn',
    Constructor: 'Ctor',
    Field: 'Fld',
    Variable: 'Var',
    Class: 'Cls',
    Interface: 'Intf',
    Module: 'Mod',
    Property: 'Prop',
    Unit: 'Unit',
    Value: 'Val',
    Enum: 'Enum',
    Keyword: 'Kwd',
    Snippet: 'Snip',
    Color: 'Clr',
    File: 'File',
    Reference: 'Ref',
    Folder: 'Dir',
    EnumMember: 'Enum',
    Constant: 'Cnst',
    Struct: 'Strct',
    Event: 'Evnt',
    Operator: 'Op',
    TypeParameter: 'Type',
  },
}

# ---------------------------------------------------------------------------
# Server configurations
# ---------------------------------------------------------------------------

def Servers(): list<dict<any>>
  return [
    {
      name: 'bashls',
      filetype: ['bash', 'sh'],
      path: 'bash-language-server',
      args: ['start'],
      workspaceConfig: {
        bashIde: {
          globPattern: '*@(.sh|.inc|.bash|.command)',
        },
      },
    },
    {
      name: 'clangd',
      filetype: ['c', 'cpp', 'objc', 'objcpp', 'cuda'],
      path: 'clangd',
      args: [],
      rootSearch: [
        '.clangd',
        '.clang-tidy',
        '.clang-format',
        'compile_commands.json',
        'compile_flags.txt',
        'configure.ac',
        '.git/',
      ],
    },
    {
      name: 'gopls',
      filetype: ['go', 'gomod', 'gowork', 'gotmpl'],
      path: 'gopls',
      args: [],
      syncInit: true,
      workspaceConfig: {
        gopls: {
          gofumpt: true,
          buildFlags: ['-tags=integration,e2e'],
        },
      },
    },
    {
      name: 'lua_ls',
      filetype: ['lua'],
      path: 'lua-language-server',
      args: [],
      rootSearch: [
        '.luarc.json',
        '.luarc.jsonc',
        '.luacheckrc',
        '.stylua.toml',
        'stylua.toml',
        'selene.toml',
        'selene.yml',
        '.git/',
      ],
      workspaceConfig: {
        Lua: {
          diagnostics: { globals: ['vim'] },
          completion: { callSnippet: 'Replace' },
          window: { progressBar: false, statusBar: false },
        },
      },
    },
    {
      name: 'ols',
      filetype: ['odin'],
      path: 'ols',
      args: [],
      rootSearch: ['ols.json', '.git/'],
    },
    {
      name: 'rust_analyzer',
      filetype: ['rust'],
      path: 'rust-analyzer',
      args: [],
      syncInit: true,
      rootSearch: ['Cargo.toml', 'rust-project.json', '.git/'],
    },
    {
      name: 'terraformls',
      filetype: ['terraform', 'terraform-vars'],
      path: 'terraform-ls',
      args: ['serve'],
      rootSearch: ['.terraform/', '.git/'],
    },
    {
      name: 'ts_ls',
      filetype: ['javascript', 'javascriptreact', 'typescript', 'typescriptreact'],
      path: 'typescript-language-server',
      args: ['--stdio'],
      initializationOptions: {
        hostInfo: 'vim',
        preferences: { disableSuggestions: true },
      },
    },
    {
      name: 'zls',
      filetype: ['zig', 'zir'],
      path: 'zls',
      args: [],
      rootSearch: ['zls.json', 'build.zig', '.git/'],
    },
  ]
enddef

# ---------------------------------------------------------------------------
# User command implementations
# ---------------------------------------------------------------------------

def LspStartCmd()
  g:LspEnable()
  echo '[LSP] Started'
enddef

def LspStopCmd()
  g:LspDisable()
  echo '[LSP] Stopped'
enddef

def LspRestartCmd()
  g:LspDisable()
  g:LspEnable()
  echo '[LSP] Restarted'
enddef

def LspToggleDocHighlightCmd()
  const cur = get(g:LspOptionsGet(), 'autoHighlight', false)
  g:LspOptionsSet({autoHighlight: !cur})
  echo '[LSP doc highlight] ' .. (cur ? 'OFF' : 'ON')
enddef

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

export def Setup()
  g:LspOptionsSet(LSP_OPTIONS)
  g:LspAddServer(Servers())

  execute printf(
    'command! -nargs=0 LspStart call %sLspStartCmd()',
    SID
  )
  execute printf(
    'command! -nargs=0 LspStop call %sLspStopCmd()',
    SID
  )
  execute printf(
    'command! -nargs=0 LspRestart call %sLspRestartCmd()',
    SID
  )
  execute printf(
    'command! -nargs=0 LspToggleDocHighlight call %sLspToggleDocHighlightCmd()',
    SID
  )

  silent! doautocmd FileType
enddef
