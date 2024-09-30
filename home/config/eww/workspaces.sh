#!/usr/bin/env zsh

function handle() {
    case $1 in
        workspacev2*) workspaces ;;
    esac
}

function workspaces() {
    active_ws="$(hyprctl activeworkspace -j | jq .id)"

    icons=(
        " "
        " "
        " "
        " "
        " "
        " "
        " "
        " "
        " "
    )

    str="(box :orientation 'v' :spacing 5"
    for i in {1..9}; do
        if [ "$i" -eq "$active_ws" ]; then
            str+="(button :onclick 'hyprctl dispatch workspace $i' :class 'active_ws' '${icons[$i]}')"
        else
            str+="(button :onclick 'hyprctl dispatch workspace $i' '${icons[$i]}')"
        fi
    done
    str+=")"

    echo -e $str
}

workspaces

socat -U - UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock |\
    while read -r line; do
        handle "$line"
    done
