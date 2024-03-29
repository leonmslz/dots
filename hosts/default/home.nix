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
    inputs.nix-colors.homeManagerModules.default

    ../../home/config
  ];

  colorScheme = inputs.nix-colors.colorSchemes.everforest;

  # Let Home-Manager Manage Itself
  programs.home-manager.enable = true;
}
