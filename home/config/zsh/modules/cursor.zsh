#!/usr/bin/env zsh
# cursor.zsh - Zsh Configuration File For Setting The Terminal Cursor

# --- Helper Function ---
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

# Reload Cursor Shape
function _fix_cursor_shape() {
    echo -ne "${CURSOR_SHAPE}"
}
precmd_functions+=(_fix_cursor_shape)

# --- Cursor Shape ---
_set_cursor_shape "steady underline"
