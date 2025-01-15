@echo off

if not exist %LOCALAPPDATA%\nvim (
mklink /J  %LOCALAPPDATA%\nvim nvim
)

if exist %LOCALAPPDATA%\zls.json (
  del %LOCALAPPDATA%\zls.json
)
copy zls.json %LOCALAPPDATA%\zls.json

@echo on
