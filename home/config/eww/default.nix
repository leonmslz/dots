# default.nix (home/config/eww) - Setup Symlinks For Eww Configuration
{ config, pkgs, ... }:

{
  home = {
    file = {
      ".config/eww" = {
        source = config.lib.file.mkOutOfStoreSymlink /home/leon/NixOS-System/home/config/eww/.;
        recursive = true;
      };
    };
    packages = with pkgs; [ eww ];
  };
}
