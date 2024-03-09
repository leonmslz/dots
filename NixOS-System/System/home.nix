# home.nix
{ config, pkgs, ... }:

let
  inherit (import ../Variables.nix)
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
    source = ../Config/Emacs;
    recursive = true;
  };
  programs.emacs.enable = true;

  # Let Home-Manager Manage Itself
  programs.home-manager.enable = true;
}
