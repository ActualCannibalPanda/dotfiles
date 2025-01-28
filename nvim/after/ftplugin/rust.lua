local bufnr = vim.api.nvim_get_current_buf()

vim.keymap.set('n', '<leader>ca', function()
  vim.cmd.RustLsp('codeAction')
end, { silent = true, buffer = bufnr })
vim.keymap.set('n', 'K', function()
  vim.cmd.RustLsp({ 'hover', 'actions' })
end, { silent = true, buffer = bufnr })

vim.bo.tabstop = 4
vim.bo.vartabstop = '4'
vim.bo.shiftwidth = 4
vim.bo.softtabstop = 4
vim.bo.expandtab = true
