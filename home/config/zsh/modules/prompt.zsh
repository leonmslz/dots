#!/usr/bin/env zsh
# prompt.zsh - Zsh Configuration File For Setting User Prompt

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
