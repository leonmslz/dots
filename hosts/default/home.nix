# home.nix
{ inputs, config, pkgs, ... }:

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
    (with pkgs; [
      emacs-gtk # Editor
      vim # Terminal Editor
      alacritty # Terminal Emulator
      firefox # Browser
      rofi-wayland # Program-Launcher
      waybar # Status-Bar
      gnome.eog # Image-Viewer
      gnome.nautilus # File-Manager
      evince # PDF-Viewer

      prismlauncher
    ]);

  # Theming
  gtk.enable = true;
  qt.enable = true;

  gtk.iconTheme.package = pkgs.papirus-icon-theme;
  gtk.iconTheme.name = "Papirus-Dark";

  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = 22;
  };

  imports = [
    inputs.hyprland.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    ../../home/config
  ];

  colorScheme = inputs.nix-colors.colorSchemes.nord;

  # Let Home-Manager Manage Itself
  programs.home-manager.enable = true;
}
