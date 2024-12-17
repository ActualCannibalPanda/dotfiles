local wezterm = require('wezterm')

local default_prog = {}
local keys = {
  { key = 'l', mods = 'ALT', action = wezterm.action.ShowLauncher },
}

local launch_menu = {}
if wezterm.target_triple == 'x86_64-pc-windows-msvc' then
  default_prog = { 'powershell.exe' }
  launch_menu = {
    {
      label = 'Micrsoft Visual Sutdio Developer Console',
      args = {
        'powershell.exe',
        '-noe',
        '-c',
        '&{Import-Module "C:/Program Files/Microsoft Visual Studio/2022/Community/Common7/Tools/Microsoft.VisualStudio.DevShell.dll"; Enter-VsDevShell  63abf6e5}',
      },
    },
  }
else
  default_prog = { '/bin/bash' }
end

return {
  keys = keys,
  default_prog = default_prog,
  launch_menu = launch_menu,
  color_scheme = 'MaterialOcean',
}
