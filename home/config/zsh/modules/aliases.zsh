#!/usr/bin/env zsh
# aliases.zsh - Zsh Configuration File Containing All Aliases

# Unalias All Existing Aliases
unalias -a

# --- Helper Function ---
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

# --- Aliases ---
# Alias Ls Commands To Eza
alias l             "eza -lah --icons --group-directories-first"   \
      lg            "eza -lah --icons --group-directories-first --git-ignore"   \
      ll            "l"                                            \
      ls            "eza -a --icons --group-directories-first"     \
      tree          "eza --tree --icons --group-directories-first" \
      treeg         "eza --tree --icons --group-directories-first --git-ignore"

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
