# home.nix
{ inputs, config, pkgs, unstablePkgs, ... }:

let
  inherit (import ../../globals.nix)
    username
    homeDir
    flakeDir
  ;
in
{
  # Basic Home-Manager Settings
  home.username = "${username}";
  home.homeDirectory = "${homeDir}";
  home.stateVersion = "23.05";

  # User Specific Packages
  home.packages =
    # (with unstablePkgs; [
      # emacs # Editor
    # ])
    # ++
    (with pkgs; [
      emacs # Editor
      vim # Terminal Editor
      alacritty # Terminal Emulator
      firefox # Browser
      rofi-wayland # Program-Launcher
      waybar # Status-Bar
      gnome.eog # Image-Viewer
      gnome.nautilus # File-Manager
      evince # PDF-Viewer
    ]);

  imports = [
    inputs.hyprland.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    ../../home/config
  ];

  colorScheme = inputs.nix-colors.colorSchemes.nord;

  # Let Home-Manager Manage Itself
  programs.home-manager.enable = true;
}
