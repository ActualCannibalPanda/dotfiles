vim.bo.tabstop = 2
vim.bo.vartabstop = '2'
vim.bo.shiftwidth = 0
vim.bo.softtabstop = -1
vim.bo.expandtab = true

if not vim.g._LuaStarted then
  vim.cmd([[
    doautocmd User LuaStarted
  ]])
end

vim.g._LuaStarted = true
