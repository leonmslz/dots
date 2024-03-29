{ inputs, config, pkgs, ... }:

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
          margin: 10px 0 0 10px;
          background-color: #${base00};
      }

      .modules-center {
          background-color: #${base00};
          margin: 0 0 0 10px;
          padding: 10px 0 10px 0;
      }

      .modules-right {
          background-color: #${base00};
          margin: 0 0 10px 10px;
          padding: 10px 0 10px 0;
      }

      #workspaces button {
          padding: 8px 8px 8px 8px;
          background-color: transparent;
      }

      #workspaces button .span {
          color: red;
      }

      #workspaces button:hover {
          background-color: #${base0D};
      }

      #workspaces button.active {
          background-color: #${base0B};
      }

      #custom-power {
          margin: 5px 0 0 4px;
          color: #${base0E};
      }

      #pulseaudio.muted {
          color: #${base0E};
      }
    '';
  };
}
