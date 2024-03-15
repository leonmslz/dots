# home.nix
{ inputs, config, pkgs, ... }:

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

  imports = [
    inputs.hyprland.homeManagerModules.default
    ../../home/config/shell.nix
    ../../home/config/hyprland.nix
    ../../home/config/waybar.nix
  ];

  # GNU Emacs Setup
  home.file.".config/emacs" = {
    source = ../../home/config/emacs;
    recursive = true;
  };
  programs.emacs.enable = true;

  # Let Home-Manager Manage Itself
  programs.home-manager.enable = true;
}
