local options = require('robertos.config.options')

local function enable_options()
  options.init()
end

vim.api.nvim_create_autocmd('User', {
  pattern = 'LazyInstall',
  callback = enable_options,
})

vim.api.nvim_create_autocmd('User', {
  pattern = 'LuaStarted',
  callback = function()
    local capabilities = require('cmp_nvim_lsp').default_capabilities()
    require('robertos.plugins.lsp.lua').setup(capabilities)
    vim.command('echomsg "here"')
  end,
})
