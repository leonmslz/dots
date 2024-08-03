# sddm.nix - Configuration File For Sddm Display Manager
{ config, pkgs, o, ... }:

let
  sddm-theme = import ./sddm-theme.nix { inherit config pkgs o; };
in
{
  # --- Sddm ---

  environment.systemPackages =
    (with pkgs.libsForQt5.qt5; [
      qtsvg qtgraphicaleffects qtquickcontrols2
    ])
    ++
    (with pkgs; [
      bibata-cursors
    ]);

  services.displayManager = {
    sddm = {
      enable = true;
      package = pkgs.sddm;

      # Wayland Support
      wayland.enable = true;
      enableHidpi = true;

      # Theming
      theme = "${sddm-theme}";

      settings.Theme = {
        CursorTheme = config.stylix.cursor.name;
        CursorSize  = config.stylix.cursor.size;
      };
    };
  };
}
