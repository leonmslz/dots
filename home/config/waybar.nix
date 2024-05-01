{ inputs, config, pkgs, scripts, ... }:

{
  programs.waybar = {
    enable = true;

    settings = [{
      layer = "top";
      position = "left";

      modules-left = ["hyprland/workspaces"];
      modules-center = ["clock"];
      modules-right = ["tray" "pulseaudio" "custom/power"];

      "hyprland/workspaces" = {
        format = "{icon}";
        format-icons = {
          "1" = "";
          "2" = "";
          "3" = "";
          "4" = "";
          "5" = "";
          "default" = "";
        };
      };

      "tray" = {
        icon-size = 15;
        spacing = 10;
      };

      "clock" = {
        format = "{:%H:%M}";
        rotate = 270;
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

    style = with config.colorScheme.palette; ''
      * {
          border: none;
          font-family: Font Awesome, Roboto, Arial, sans-serif;
          font-size: 13px;
          color: #ffffff;
          border-radius: 5px;
      }

      window#waybar {
          background: rgba(0, 0, 0, 0);
      }

      .modules-left {
          background-color: #${base00};
          /* top | right | bottom | left */
          margin: 10px 0 0 10px;
      }

      .modules-center {
          background-color: #${base00};
          /* top | right | bottom | left */
          margin: 0 0 0 10px;
      }

      .modules-right {
          background-color: #${base00};
          /* top | right | bottom | left */
          margin: 0 0 10px 10px;
      }

      #workspaces button {
          padding: 8px 8px 8px 8px;
      }

      #workspaces button:hover {
          background-color: #${base0D};
      }

      #workspaces button.active {
          background-color: #${base0B};
      }

      #clock {
          background-color: #${base07};
          /* top | right | bottom | left */
          padding: 10px 0 10px 0;
      }

      #tray {
          background-color: #${base0D};
          /* top | right | bottom | left */
          padding: 10px 0 10px 0;
      }

      #pulseaudio {
          background-color: #${base02};
          /* top | right | bottom | left */
          padding: 10px 0 10px 0;
      }

      #pulseaudio.muted {
          color: #${base0E};
      }

      #custom-power {
          /* top | right | bottom | left */
          padding: 10px 0 10px 0;
          background-color: #${base0E};
      }
    '';
  };
}
