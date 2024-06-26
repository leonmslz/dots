# sddm.nix - Configuration File For Sddm Display Manager
{ config, pkgs, o, ... }:

let
  sddm-theme = import ./sddm-theme.nix { inherit pkgs o; };
in
{
  # --- Sddm ---

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
