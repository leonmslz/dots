{ config, ... }:

{
  home.file.".zshrc" = {
    source = config.lib.file.mkOutOfStoreSymlink /home/leon/NixOS-System/home/config/zsh/rc.zsh;
  };
}
