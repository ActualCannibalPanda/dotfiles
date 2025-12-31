#!/bin/sh

export EDITOR="nvim"

# follow XDG base dir specification
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

# bootstrap .zshrc to ~/.config/zsh/.zshrc, any other zsh config files can also reside here
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# history files
export LESSHISTFILE="$XDG_CACHE_HOME/less_history"
export PYTHON_HISTORY="$XDG_DATA_HOME/python/history"

# moving other files and some other vars
export XINITRC="$XDG_CONFIG_HOME/x11/xinitrc"
export XPROFILE="$XDG_CONFIG_HOME/x11/xprofile"
export XRESOURCES="$XDG_CONFIG_HOME/x11/xresources"
export XSERVERRC="$XDG_CONFIG_HOME/x11/xserverrc"
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export GOPATH="$XDG_DATA_HOME/go"
export GOBIN="$GOPATH/bin"
export GOMODCACHE="$XDG_CACHE_HOME/go/mod"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export GRADLE_USER_HOME="$XDG_DATA_HOME/gradle"
export NUGET_PACKAGES="$XDG_CACHE_HOME/NuGetPackages"
export PARALLEL_HOME="$XDG_CONFIG_HOME/parallel"
export FFMPEG_DATADIR="$XDG_CONFIG_HOME/ffmpeg"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc":"$XDG_CONFIG_HOME/gtk-2.0/gtkrc.mine"
export WINEPREFIX="$XDG_DATA_HOME/wineprefixes/default"
export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME/jupyter"
export LEIN_HOME="$XDG_DATA_HOME/lein"
export MAVEN_OPTS='-Dmaven.repo.local="$XDG_DATA_HOME/maven/repository"'
export MYPY_CACHE_DIR="$XDG_CACHE_HOME/mypy"
export NUGET_PACKAGES="$XDG_CACHE_HOME/NuGetPackages"
export NVM_DIR="$XDG_DATA_HOME/nvm"
export RENPY_PATH_TO_SAVES="$XDG_DATA_HOME/renpy"
export RENPY_MULTIPERSISTENT="$XDG_DATA_HOME/renpy_shared"
export VSCODE_PORTABLE="$XDG_DATA_HOME/vscode"
