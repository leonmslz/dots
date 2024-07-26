# default.nix (home/config/zsh) - Setup Symlinks For Zsh Configuration
{ config, ... }:

{
  home = {
    file = {
      ".config/zsh" = {
        source = config.lib.file.mkOutOfStoreSymlink /home/leon/NixOS-System/home/config/zsh/.;
        recursive = true;
      };
      ".zshrc" = {
        source = config.lib.file.mkOutOfStoreSymlink /home/leon/.config/zsh/rc.zsh;
      };
    };
  };
}
