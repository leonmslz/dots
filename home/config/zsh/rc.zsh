#!/usr/bin/env zsh
# rc.zsh - Zsh Main Configuration File

# Source Modules
local module_dir="${HOME}/.config/zsh/modules"

source "${module_dir}/basics.zsh"
source "${module_dir}/plugins.zsh"
source "${module_dir}/cursor.zsh"
source "${module_dir}/prompt.zsh"
source "${module_dir}/aliases.zsh"
source "${module_dir}/autostart.zsh"
