# hyprpaper.nix - Nix Declarative Configuration File For Hyprpaper Wallpaper Utility
{ config, pkgs, o, ... }:

{
  services.hyprpaper = {
    enable = true;
    package = pkgs.hyprpaper;

    settings =
      let
        wallpaper = "${config.stylix.image}";
      in
        {
          ipc       = "on";
          splash    = false;
          preload   = [ "${wallpaper}" ];
          wallpaper = [ "DP-1,${wallpaper}" ];
        };
  };
}
