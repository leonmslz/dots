#!/usr/bin/env zsh
# .zshrc
#  ______    _     _____   _____
# |___  /   | |   |  __ \ / ____|
#    / / ___| |__ | |__) | |
#   / / / __| '_ \|  _  /| |
#  / /__\__ \ | | | | \ \| |____
# /_____|___/_| |_|_|  \_\\_____|
#
# Configuration File For Z-Shell
# By Leon Schulz

# Utility Functions -------------------------------------------------------------------------------

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

# Basics ------------------------------------------------------------------------------------------

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

# Auto-Completion ---------------------------------------------------------------------------------

autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

# History -----------------------------------------------------------------------------------------

HISTSIZE=10000000
SAVEHIST=10000000

zsh_cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/."
mkdir -p $zsh_cache_dir
HISTFILE="${zsh_cache_dir}/history"

# Aliases -----------------------------------------------------------------------------------------

# Unalias All Existing Aliases
unalias -a

# Wrapper-Function For Managing Aliases
function alias() {
    if (( $# % 2 == 0 && $# != 0 )); then
        for (( i = 1; i <= $#; i += 2 )); do
            eval "builtin alias $argv[$i]='$argv[$i+1]'"
        done
        return 0
    fi
    builtin alias $@
}

# Alias Ls Commands To Eza
alias l             "eza -lah --icons --group-directories-first"   \
      ll            "l"                                            \
      ls            "eza -a --icons --group-directories-first"     \
      tree          "eza --tree --icons --group-directories-first"

# Git Related Aliases
alias g             "git"                                          \
      ga            "git add"                                      \
      gaa           "git add ."                                    \
      gs            "git status"                                   \
      gc            "git commit -m"                                \
      gl            "git pull"                                     \
      gp            "git push"

# Aliases For Reloading Zsh
alias zsh           "clear && exec zsh -l"                         \
      zsh-no-config "clear && exec zsh -l -f"


# NixOS Specific Aliases
if is_installed nix; then

    alias nix-rebuild "sudo nixos-rebuild switch --flake ~/NixOS-System/.#default" \
          nix-cleanup "sudo nix-collect-garbage --delete-older-than 1d"

fi

# Prompt ------------------------------------------------------------------------------------------

function precmd() {

    # Prompt Symbol And Outline Based On Error Code
    case $? in
        0) # Success
            exit_code_color="cyan"
            exit_code_symbol=""
            ;;
        127) # Command doesn't exist
            exit_code_color="red"
            exit_code_symbol=""
            ;;
        *) # Other Error
            exit_code_color="magenta"
            exit_code_symbol=""
            ;;
    esac
    outline_begin_module="%F{${exit_code_color}}( %F{reset}"
    outline_end_module="%F{${exit_code_color}})$exit_code_symbol  %F{reset}"

    # Display Git Branch When Inside A Git Repository
    git_branch_module=""
    if git_branch=$(basename $(git symbolic-ref HEAD 2>/dev/null) 2>&-); then
        git_branch_module="%F{green} $git_branch%F{reset} "
    fi

    # Display Shell
    shell_name=$(basename -- $SHELL)
    current_shell_module="%F{yellow}  %F{green}$shell_name %F{reset}"

    # Display The Current Working Directory
    working_directory_module="%F{red} %F{magenta}%~ %F{reset}"

    # Construct Prompt
    PROMPT=$(echo -ne $outline_begin_module     \
                      $current_shell_module     \
                      $working_directory_module \
                      $git_branch_module        \
                      $outline_end_module       )
}

# Cursor ------------------------------------------------------------------------------------------

function _set_cursor_shape() {
    case ${1:-"blinking block"} in
        "blinking block")
            CURSOR_SHAPE='\e[1 q'
            ;;
        "steady block")
            CURSOR_SHAPE='\e[2 q'
            ;;
        "blinking underline")
            CURSOR_SHAPE='\e[3 q'
            ;;
        "steady underline")
            CURSOR_SHAPE='\e[4 q'
            ;;
        "blinking bar")
            CURSOR_SHAPE='\e[5 q'
            ;;
        "steady bar")
            CURSOR_SHAPE='\e[6 q'
            ;;
        *)
            log_msg "Unknown cursor shape \`$1\`."
            ;;
    esac
}
_set_cursor_shape

function _fix_cursor_shape() {
    echo -ne "${CURSOR_SHAPE}"
}
precmd_functions+=(_fix_cursor_shape)

_set_cursor_shape "steady underline"

# Plugins -----------------------------------------------------------------------------------------

function plugin() {
    repo_name=$1
    plugin_url="https://github.com/$repo_name/"
    plugin_name=$(basename $repo_name)
    plugin_dir=${zsh_cache_dir}/$plugin_name

    if [ -d $plugin_dir ]; then
        source $plugin_dir/$plugin_name.plugin.zsh
        source $plugin_dir/$plugin_name.zsh
    else
        log_msg "Installing '$repo_name'..."
        git clone ${plugin_url} $plugin_dir
        zsh
    fi
}

plugin "zsh-users/zsh-autosuggestions"
plugin "zsh-users/zsh-syntax-highlighting"

# Autostart ---------------------------------------------------------------------------------------

is_installed pfetch && pfetch
