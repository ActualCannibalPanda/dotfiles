# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Set the directory for zinit
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

# Download zinit if it doesn't exist
if [ ! -d "$ZINIT_HOME" ]; then
	mkdir -p "$(dirname $ZINIT_HOME)"
	git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

# Start zinit
source "${ZINIT_HOME}/zinit.zsh"

# Linux Variables
export EDITOR=/usr/bin/nvim

# Install Powerlevel10k``
zinit ice depth=1; zinit light romkatv/powerlevel10k

# Add in zsh plugins
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light Aloxaf/fzf-tab

# Add in snippets
zinit snippet OMZP::git
zinit snippet OMZP::sudo
zinit snippet OMZP::archlinux

# Load Modules
autoload -U compinit && compinit
autoload -U colors && colors

zinit cdreplay -q

# Keybindings
bindkey -e
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward
bindkey ' ' magic-space

# History
HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=$HISTSIZE
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# Completion styling
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu no
zstyle ":fzf-tab:complete:cd:*" fzf-preview 'ls --color $realpath'

# Aliases
alias ls='ls --color'

# Alias suffixes
alias -s py="$EDITOR"
alias -s c="$EDITOR"
alias -s cpp="$EDITOR"
alias -s cs="$EDITOR"
alias -s go="$EDITOR"

# Shell integrations
eval "$(fzf --zsh)"

# Open buffer line in editor
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

# chpwd hooks
autoload -Uz add-zsh-hook

# Used to determine where the virutal environment 
# really is
export VENV=
function auto_venv() {
  if [[ -n "$VIRTUAL_ENV" ]]; then
    if [[ ! "$PWD" =~ "^$VENV" ]]; then
      export VENV=
      deactivate
    fi
  fi

  [[ -n $VIRTUAL_ENV ]] && return

	local dir="$PWD"
	while [[ "$dir" != "/" ]]; do
		if [[ -f "$dir/.venv/bin/activate" ]]; then
      export VENV="$dir"
			source "$dir/.venv/bin/activate"
			return
		fi
    if [[ -f "$dir/venv/bin/activate" ]]; then
      export VENV="$dir"
      source "$dir/venv/bin/activate"
      return
    fi
		dir="${dir:h}"
	done
}

add-zsh-hook chpwd auto_venv

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f "$ZDOTDIR/.p10k.zsh" ]] && source "$ZDOTDIR/.p10k.zsh"
