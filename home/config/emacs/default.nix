# default.nix (home/config/emacs) - Setup Symlinks For Emacs Configuration
{ config, pkgs, ... }:

{
  home = {
    file = {
      ".config/emacs" = {
        source = config.lib.file.mkOutOfStoreSymlink /home/leon/NixOS-System/home/config/emacs/.;
        recursive = true;
      };
    };
    packages = with pkgs; [ emacs29-pgtk ispell ];
  };
}
