{ pkgs, ... }:

{
  # --- Sddm ---

  services.displayManager = {
    defaultSession = "hyprland";
    sddm =
      let
        sddm-theme = import ./sddm-theme.nix { inherit pkgs; };
      in
        {
          enable = true;
          wayland.enable = true;
          enableHidpi = true;
          theme = "${sddm-theme}";
        };
  };

}
