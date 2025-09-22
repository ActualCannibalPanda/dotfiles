local wezterm = require('wezterm')

local config = {}

config.default_prog = { 'nu' }
config.font = wezterm.font('Fira Code Nerd Font')
config.color_scheme = 'AdventureTime'
config.window_background_opacity = 0.88
config.use_fancy_tab_bar = false

return config
