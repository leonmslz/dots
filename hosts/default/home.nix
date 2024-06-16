# home.nix - Home-Manger Configuration File
{ inputs, config, pkgs, scripts, lib, o, ... }:

{
  imports = [
    # ./options.nix

    inputs.hyprland.homeManagerModules.default
    inputs.nix-colors.homeManagerModules.default

    ../../home/config
  ];

  # Basic Home-Manager Settings
  home.username = "${o.username}";
  home.homeDirectory = "/home/${o.username}";
  home.stateVersion = "23.05";

  # User Specific Packages
  home.packages =
    (with pkgs; [
      emacs29-pgtk # Editor
      vim # Terminal Editor
      alacritty # Terminal Emulator
      firefox # Browser
      rofi-wayland # Program-Launcher
      waybar # Status-Bar
      gnome.eog # Image-Viewer
      gnome.nautilus # File-Manager
      evince # PDF-Viewer
      qrencode # QR-Code Utility
      prismlauncher # Minecraft-Launcher
      networkmanagerapplet # Network-Manager-Applet
      bottles # Wine-Manager
      wine-wayland
      fastfetch # Fetching Tool
      hyprpicker # Color-Picker Tool
      # processing
      nix-prefetch-scripts # Collection of all the nix-prefetch-* scripts which may be used to obtain source hashes
      cider # Apple Music
    ])
    ++
    (with scripts; [
      hic
      rofi-logout-menu
    ]);

  # Qt-Theming
  qt.enable = true;

  # Gtk-Theming
  gtk = {
    enable = true;
    theme = {
      package = pkgs.orchis-theme;
      name = "Orchis-Dark";
    };
    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus-Dark";
    };
  };

  # Cursor-Theming
  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Classic";
    size = lib.mkForce 22;
  };

  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-dark-medium;

  # Let Home-Manager Manage Itself
  programs.home-manager.enable = true;
}
