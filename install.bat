@echo off

if not exist %LOCALAPPDATA%\nvim (
mklink /J  %LOCALAPPDATA%\nvim nvim
)

if not exist %USERPROFILE%\.emacs.d (
mklink /J  %USERPROFILE%\.emacs.d .emacs.d
)

if exist %LOCALAPPDATA%\zls.json (
  del %LOCALAPPDATA%\zls.json
)
copy zls.json %LOCALAPPDATA%\zls.json

@echo on
