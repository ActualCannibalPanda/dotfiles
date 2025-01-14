local options = require('robertos.config.options')

local function enable_options()
  options.init()
end

vim.api.nvim_create_autocmd('User', {
  pattern = 'LazyInstall',
  callback = enable_options,
})
vim.api.nvim_create_autocmd({ 'BufEnter', 'BufWinEnter' }, {
  callback = enable_options,
})
