{ config, ... }:

{
  home.file.".config/emacs" = {
    source = config.lib.file.mkOutOfStoreSymlink /home/leon/NixOS-System/home/config/emacs/.;
    recursive = true;
  };
}
