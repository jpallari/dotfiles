local function mapk(lhs, rhs, desc)
  vim.keymap.set('n', lhs, rhs, { remap = true, buffer = true, desc = desc })
end

mapk('l', '<cr>', 'Open file/directory')
mapk('h', '-^', 'Go up a directory')
mapk('\\', '<cmd>Lexplore<cr>', 'Close file explorer')

vim.opt_local.bufhidden = 'wipe'
