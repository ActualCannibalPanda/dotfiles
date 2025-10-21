local wezterm = require('wezterm')

local config = {}

config.default_prog = { 'nu' }
config.font = wezterm.font('Fira Code Nerd Font')
config.color_scheme = 'AdventureTime'
config.window_background_opacity = 0.88
config.use_fancy_tab_bar = false

config.set_environment_variables = {
  PATH =  wezterm.home_dir .. "/.cargo/bin/:" .. os.getenv "PATH",
}

return config
