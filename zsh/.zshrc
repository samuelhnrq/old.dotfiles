#!/bin/zsh

# Sane defaults, yes ZSH I would like command history.
HISTFILE=~/.zhistory
SAVEHIST=3000
setopt share_history
setopt extended_glob
setopt auto_pushd

autoload colors # Loads the escape codes into the $fg and $bg maps
colors
TERM=xterm-256color
# Prompt...
PS1="[%{$fg[blue]%}%n%{$fg[default]%}] %{$fg[yellow]%}%1~%{$fg[default]%} $ "
RPS1="%T"

if [ -d ~/.zplug ] && [ -f ~/.zplugrc ]; then
    source ~/.zplugrc
else
    echo "WARNING ZPLUG NOT FOUND!!!"
fi


