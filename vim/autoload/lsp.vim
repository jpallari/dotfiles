vim9script

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
# LSP attach/detach hooks
# ---------------------------------------------------------------------------

def OnAttach()
  # Key bindings
  nnoremap <buffer> <silent> K           <cmd>LspHover<cr>
  nnoremap <buffer> <silent> <C-]>       <cmd>LspGotoDefinition<cr>
  nnoremap <buffer> <silent> grD         <cmd>LspGotoDeclaration<cr>
  nnoremap <buffer> <silent> gra         <cmd>LspCodeAction<cr>
  xnoremap <buffer> <silent> gra         :LspCodeAction<cr>
  nnoremap <buffer> <silent> gri         <cmd>LspGotoImpl<cr>
  nnoremap <buffer> <silent> grn         <cmd>LspRename<cr>
  nnoremap <buffer> <silent> grr         <cmd>LspShowReferences<cr>
  nnoremap <buffer> <silent> grt         <cmd>LspGotoTypeDef<cr>
  nnoremap <buffer> <silent> grx         <cmd>LspCodeLens<cr>
  nnoremap <buffer> <silent> gO          <cmd>LspDocumentSymbol<cr>
  inoremap <buffer> <silent> <C-S>       <cmd>LspShowSignature<cr>
  nnoremap <buffer> <silent> <leader>ca  <cmd>LspCodeAction<cr>
  nnoremap <buffer> <silent> <leader>cf  <cmd>LspFormat<cr>
  nnoremap <buffer> <silent> <leader>cl  <cmd>LspCodeLens<cr>
  nnoremap <buffer> <silent> <leader>cs  <cmd>LspShowSignature<cr>
  nnoremap <buffer> <silent> <leader>cr  <cmd>LspRename<cr>
  nnoremap <buffer> <silent> <leader>tdh <cmd>LspToggleDocHighlight<cr>
  nnoremap <buffer> <silent> <leader>tih <cmd>LspInlayHints toggle<cr>
  nnoremap <buffer> <silent> <leader>dd  <cmd>LspDiag current<cr>
  nnoremap <buffer> <silent> ]d          <cmd>LspDiag next<cr>
  nnoremap <buffer> <silent> [d          <cmd>LspDiag prev<cr>
enddef

def OnDetach()
  # Key bindings
  silent! nunmap <buffer> K
  silent! nunmap <buffer> <C-]>
  silent! nunmap <buffer> grD
  silent! nunmap <buffer> gra
  silent! xunmap <buffer> gra
  silent! nunmap <buffer> gri
  silent! nunmap <buffer> grn
  silent! nunmap <buffer> grr
  silent! nunmap <buffer> grt
  silent! nunmap <buffer> grx
  silent! nunmap <buffer> gO
  silent! iunmap <buffer> <C-S>
  silent! nunmap <buffer> <leader>ca
  silent! nunmap <buffer> <leader>cf
  silent! nunmap <buffer> <leader>cl
  silent! nunmap <buffer> <leader>cs
  silent! nunmap <buffer> <leader>cr
  silent! nunmap <buffer> <leader>tdh
  silent! nunmap <buffer> <leader>tih
  silent! nunmap <buffer> <leader>dd
  silent! nunmap <buffer> ]d
  silent! nunmap <buffer> [d
enddef

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

export def Setup()
  g:LspOptionsSet(LSP_OPTIONS)
  g:LspAddServer(Servers())

  command! -nargs=0 LspStart LspStartCmd()
  command! -nargs=0 LspStop LspStopCmd()
  command! -nargs=0 LspRestart LspRestartCmd()
  command! -nargs=0 LspToggleDocHighlight LspToggleDocHighlightCmd()

  augroup jp_lsp
    autocmd!
    autocmd User LspAttached OnAttach()
    autocmd User LspDetached OnDetach()
  augroup END

  silent! doautocmd FileType
enddef
