#!/bin/zsh

# .zshrc
# Configuration File For Zsh

# Stop freezing terminal
stty stop undef

# Better completions
autoload -Uz compinit

zstyle ':completion:*' menu select

zmodload zsh/complist

_comp_options+=(globdots) # Include hidden files

compinit -C -d $HOME/.cache/zsh/zcompdump

zle_highlight=('paste:none') # Don't highlight pasted text

setopt auto_cd               # Enable Autocd
setopt interactive_comments  # Enable Interactive Comments

# Set Global Variables
export EDITOR=${EDITOR:-"nvim"}
export VISUAL=${VISUAL:-"nvim"}
export USER=${USER:-$(whoami)}
# export MANPAGER="sh -c 'col -bx | bat -l man -p'"

export PATH="${PATH}:/home/leon/.local/bin"

# Emacs Like Keybindings For Zsh Line Editor (ZSH)
bindkey -e

bindkey -e "^k"  kill-buffer
bindkey -e "^xk" kill-line
bindkey -e "^s"  history-incremental-search-backward

# Generate Prompt
genprompt() {

    # Save Status-Code Of Last Run Command
    local exit_status=$?

    PRE_PS1=""
    if test $exit_status -ne 0; then
        PRE_PS1+=$(colorize red bold "[!] ")
    else
        PRE_PS1+=$(colorize green bold "[=] ")
    fi

    # Indication If Run Inside A Docker Container
    if test -f "/.dockerenv"; then
        PRE_PS1+=$(colorize red bold "(Docker) ")
    fi

    PRE_PS1+=$(colorize yellow normal "$USER ")
    PRE_PS1+=$(colorize magenta normal "@ ")
    PRE_PS1+=$(colorize cyan normal "$(</etc/hostname) ")
    PRE_PS1+=$(colorize blue normal "in ")

    if test $PWD = $HOME; then
        PRE_PS1+=$(colorize green normal "~ ")
    else
        PRE_PS1+=$(colorize green normal "${PWD##*/} ")
    fi

    # Indication (Displaying Current Branch) If Inside a Git Repository
    if git rev-parse --git-dir &>/dev/null; then
        PRE_PS1+=$(colorize red normal "($(git rev-parse --abbrev-ref HEAD)) ")
    fi

    PRE_PS1+=$(colorize red normal "|$(date +%H:%M)")

    # Printing Pre-Prompt
    echo ""
    echo $PRE_PS1

    # Setting Actual Prompt
    PS1=$(colorize magenta bold ">> ")
}

# Set Cursor
setcursor() {
    # Change Cursor To _
    printf "\e[4 q"
}

# Initializing Function `precmd` Which Gets Run Every Time
# Before Your Prompt Gets Printed
precmd() {
    genprompt
    setcursor
}

# LS-Aliases
alias ls="exa --icons -a --group-directories-first"
alias ll="exa --icons -a --group-directories-first -l"
alias tree="exa --icons --tree -a"
alias tree3="exa --icons --tree -a --level=3"

# Cat-Aliases
alias cat="bat --theme=OneHalfDark"

# Editor Related Aliases
alias vi="$EDITOR"
alias iv="$EDITOR"
alias vim="$EDITOR"
alias ivm="$EDITOR"

# Aliases to Source And Open RC
alias rc="$EDITOR $HOME/.zshrc"
alias so="exec ${SHELL##*/}"

# Function to Clear the Screen
clear() {
    printf "\e[H\e[2J"
}
alias c="clear"

# Docker Related Functions
connect() {
    checkargs $0 $# 1 || return 1

    local container=$1
    
    if docker ps -a --filter status=running | grep $container >/dev/null; then
        docker attach $container
    else
        docker start -ai $container
    fi
}

throwaway() {
    checkargs $0 $# 1 || return 1

    local container=$1

    docker run --rm -it $container 
}

rebuild() {
    checkargs $0 $# 1 || return 1

    local tag=$1

    docker build -t "$tag" .
}

alias list="docker ps --all"
alias listi="docker images"
alias listr="docker ps --all --filter status=running"

# Function to Extract An Archive
ex() { 
    if test $# -lt 1; then
        errormsg $0 "Need At Least One Argument!"
    fi

    for archive in "$@"; do 
        if [ -f "$archive" ] ; then 
            case $archive in 
                *.tar.bz2)   tar xvjf $archive    ;; 
                *.tar.gz)    tar xvzf $archive    ;; 
                *.bz2)       bunzip2 $archive     ;; 
                *.rar)       rar x $archive       ;; 
                *.gz)        gunzip $archive      ;; 
                *.tar)       tar xvf $archive     ;; 
                *.tbz2)      tar xvjf $archive    ;; 
                *.tgz)       tar xvzf $archive    ;; 
                *.zip)       unzip $archive       ;; 
                *.Z)         uncompress $archive  ;; 
                *.7z)        7z x $archive        ;; 
                *)           errormsg $0 "Not Able To Extract \`$archive\`!" ;; 
            esac 
        else 
            errormsg $0 "\`$archive\` Is Not A Valid File!"
        fi 
    done 
}

# Function to Check the Amount Of Arguments Provided
checkargs() {

    if test $# -ne 3; then
        colorize red bold "Error: Wrong number of Arguments!"
    fi

    local functionName=$1
    local providedArgs=$2
    local expectedArgs=$3

    if test $providedArgs -ne $expectedArgs; then

        errormsg $functionName "Wrong Number Of Arguments! Need $expectedArgs, But Got $providedArgs!"

        return 1
    fi
}

errormsg() {

        local errorMessage=""

        errorMessage+=$(colorize red bold "[Error]")
        errorMessage+=": "
        errorMessage+=$(colorize yellow normal "\`")
        errorMessage+=$1
        errorMessage+=$(colorize yellow normal "\`")
        errorMessage+=": "
        errorMessage+=$(colorize magenta normal $2)

        echo $errorMessage
        
        return 1
}

# Function to Check If a Program is Installed
isinstalled() {

    checkargs $0 $# 1 || return 1

    local program=$1

    if command -v $program >/dev/null; then
        return 0
    fi
    return 1
}

colorize red bold "Loaded ZSH-Config!"
