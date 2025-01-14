@echo off

if not exist %LOCALAPPDATA%\nvim (
mklink /J  %LOCALAPPDATA%\nvim nvim
)

if not exist %LOCALAPPDATA%\zls.json (
  mklink %LOCALAPPDATA%\zls.json zls.json
)

@echo on
