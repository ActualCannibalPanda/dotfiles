-- lazy nvim should load first
require('robertos.config.lazy')
-- load other config files
require('robertos.config.autocmd')
require('robertos.config.colorscheme')
require('robertos.config.diagnostics')
require('robertos.config.keymaps')
require('robertos.config.options').init()

if vim.loop.os_uname().sysname == "Windows_NT" then
  require("nvim-treesitter.install").compilers = { "zig" }
end
