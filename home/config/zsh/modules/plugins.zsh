#!/usr/bin/env zsh
# plugins.zsh - Zsh Configuration File For Installing Plugins

# --- Helper Function ---
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

# --- Plugins ---
plugin "zsh-users/zsh-autosuggestions"     # Auto Complete
plugin "zsh-users/zsh-syntax-highlighting" # Syntax Highlighting
