{ inputs, config, pkgs, scripts, ... }:

{
  programs.waybar = {
    enable = true;

    settings = [{
      layer = "top";
      position = "left";

      modules-left = ["custom/nix-icon" "hyprland/workspaces"];
      modules-center = ["clock"];
      modules-right = ["tray" "pulseaudio" "custom/power"];

      "custom/nix-icon" = {
        format = "󱄅";
      };

      "hyprland/workspaces" = {
        format = "{icon}";
        format-icons = {
          "1" = "";
          "2" = "";
          "3" = "";
          "4" = "";
          "5" = "";
          "default" = "";
        };
        persistent-workspaces = {
          "*" = [ 1 2 3 4 ];
        };
      };

      "tray" = {
        icon-size = 15;
        spacing = 10;
      };

      "clock" = {
        format = "{:%H\n%M\n\n%d\n%m\n%y}";
      };

      "pulseaudio" = {
        format = "{volume}% {icon}";
        format-icons = {
          default = ["" "" ""];
        };
        rotate = 270;
      };

      "custom/power" = {
        format = "";
        on-click = "${scripts.rofi-logout-menu}/bin/rofi-logout-menu";
      };
    }];

    style = ./style.css;
  };
}
