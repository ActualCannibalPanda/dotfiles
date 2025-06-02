local bufnr = vim.api.nvim_get_current_buf()
local s = vim.keymap.set
s('n', '<leader>ra', function()
  vim.cmd.RustLsp('codeAction')
end, { silent = true, buffer = bufnr })

s('n', 'K', function()
  vim.cmd.RustLsp({ 'hover', 'actions' })
end, { silent = true, buffer = bufnr })

s('n', '<leader>rr', function()
  vim.cmd.RustLsp { 'runnables', bang = true }
end, { buffer = bufnr, desc = 'Run Rust Project' })

s('n', '<leader>rd', function()
  vim.cmd.RustLsp { 'debuggables', bang = true }
end, { buffer = bufnr, desc = 'Debug Rust Project' })

s('n', '<leader>re', function()
  vim.cmd.RustLsp('explainError')
end, { buffer = bufnr, desc = 'Explain Rust Error' })

s('n', '<leader>rm', function()
  vim.cmd.RustLsp('expandMacro')
end, { buffer = bufnr, desc = 'Expand Rust Macro' })

vim.bo.tabstop = 4
vim.bo.vartabstop = '4'
vim.bo.shiftwidth = 4
vim.bo.softtabstop = 4
vim.bo.expandtab = true
