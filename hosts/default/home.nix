# home.nix
{ config, pkgs, ... }:

let
  inherit (import ../../globals.nix)
    username
    homeDir
  ;
in
{
  # Basic Home-Manager Settings
  home.username = "${username}";
  home.homeDirectory = "${homeDir}";
  home.stateVersion = "23.05";

  # GNU Emacs Setup
  home.file.".config/emacs" = {
    source = ../../home/config/emacs;
    recursive = true;
  };
  programs.emacs.enable = true;

  imports = [
    ../../home/config/shell.nix
  ];

  # Let Home-Manager Manage Itself
  programs.home-manager.enable = true;
}
