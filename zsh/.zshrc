#!/bin/zsh
if [ -d ~/.zplug ] && [ -f ~/.zplugrc ]; then
    source ~/.zplugrc
else
    echo "WARNING ZPLUG NOT FOUND!!!"
fi

ZSH_THEME="dracula"

# Sane defaults, yes ZSH I would like command history.
HISTFILE=~/.zhistory
SAVEHIST=3000
setopt share_history
setopt inc_append_history HIST_IGNORE_DUPS HIST_SAVE_NO_DUPS
setopt extended_glob
setopt auto_pushd

# Key binds
bindkey -e
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line
bindkey '^[[3~' delete-char
bindkey '^[[1;5C' vi-forward-word
bindkey '^[[1;5D' vi-backward-word

# Aliases
alias vim=nvim

# functions
mkdcd() {
    mkdir -p $1
    cd $1
}

