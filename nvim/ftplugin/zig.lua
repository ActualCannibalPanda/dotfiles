local bufnr = vim.api.nvim_get_current_buf()
local bo = vim.bo[bufnr]
bo.tabstop = 4
bo.vartabstop = '4'
bo.shiftwidth = 4
bo.softtabstop = 4
bo.expandtab = true
