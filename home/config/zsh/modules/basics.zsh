#!/usr/bin/env zsh
# basics.zsh - Basic Settings For Zsh Shell

# --- Utility Functions ---
# Color Codes
RED='\e[31m'
GREEN='\e[32m'
YELLOW='\e[33m'
BLUE='\e[34m'
MAGENTA='\e[35m'
CYAN='\e[36m'
RESET='\e[0m'

# Check If A Program Is Installed
function is_installed() {
    type $1 >/dev/null
}

# Log-Message
function log_msg() {
    echo -e "[${GREEN}LOG${RESET}]: $1"
}

# --- Basics Settings ---
# Stop Terminal From Freezing
stty stop undef

# Interactive Comments
setopt interactive_comments

# Correct Typos
setopt correctall

# Auto-Cd
setopt auto_cd

function add_to_cd_path {
    cdpath+=($1)
}
add_to_cd_path "$HOME"
add_to_cd_path "$HOME/.config/."

# --- Auto-Completion ---
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

# --- History ---
HISTSIZE=10000000
SAVEHIST=10000000

zsh_cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/."
mkdir -p $zsh_cache_dir
HISTFILE="${zsh_cache_dir}/history"
