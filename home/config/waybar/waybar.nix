# waybar.nix - Nix Declarative Configuration File For Waybar Status-Bar
{ inputs, config, lib, pkgs, scripts, ... }:

{
  programs.waybar = {
    enable = true;

    settings = [{
      layer = "top";
      position = "left";

      modules-left = [
        "custom/nix-icon"
        "hyprland/workspaces"
      ];

      modules-right = [
        "tray"
        "pulseaudio"
        "pulseaudio/slider"
        "clock#time"
        "clock#date"
        "custom/power"
      ];

      "custom/nix-icon" = {
        format = "󱄅";
        tooltip = false;
      };

      "hyprland/workspaces" = {
        format = "{icon}";
        format-icons = {
          "1" = " ";
          "2" = " ";
          "3" = " ";
          "4" = " ";
          "5" = "";
          "default" = " ";
        };
        persistent-workspaces = {
          "*" = [ 1 2 3 4 ];
        };
      };

      "tray" = {
        icon-size = 15;
        spacing = 10;
      };

      "clock#time" = {
        format = "{:%H\n%M}";
      };

      "clock#date" = {
        format = "{:%d\n%m\n%y}";
      };

      "pulseaudio" = {
        ignored-sinks = ["Easy Effects Sink"];
        format = "{icon}";
        format-muted = "󰝟";
        tooltip = true;
        tooltip-format = "{volume}%";
        format-icons = {
          default = ["󰕿" "󰖀" "󰕾"];
        };
      };

      "pulseaudio/slider" = {
        ignored-sinks = ["Easy Effects Sink"];
        min = 0;
        max = 100;
        orientation = "vertical";
      };

      "custom/power" = {
        format = " ";
        on-click = "${scripts.rofi-logout-menu}/bin/rofi-logout-menu";
        tooltip = false;
      };
    }];

    style = with config.lib.stylix.colors; ''
      @define-color background #${base01};
      @define-color foreground #${base06};
      @define-color border     #${base03};
      @define-color hover      #${base02};
      @define-color selected   #${base0B};
      @define-color slider     #${base0A};
      @define-color highlight  #${base0D};
      @define-color logout     #${base0E};

      @import "${./style.css}";
    '';
  };
}
