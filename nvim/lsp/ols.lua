---@brief
---
--- https://github.com/DanielGavin/ols
---
--- `Odin Language Server`.

---@type vim.lsp.Config
return {
  cmd = { 'ols' },
  filetypes = { 'odin' },
  root_markers = { 'ols.json', '.git', '.git' },
}
