-- lazy nvim should load first
require('robertos.config.lazy')
-- load other config files
require('robertos.config.autocmd')
require('robertos.config.diagnostics')
require('robertos.config.keymaps')
require('robertos.config.options').init()
