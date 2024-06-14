# sddm.nix - Configuration File For Sddm Display Manager
{ pkgs, ... }:

let
  sddm-theme = import ./sddm-theme.nix { inherit pkgs; };
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
      settings.Theme.CursorTheme = "Bibata-Modern-Classic";
      settings.Theme.CursorSize = 22;
    };
  };
}
