{ inputs, config, pkgs, lib, ... }:

{
  environment.systemPackages =
    (with pkgs.greetd; [
      greetd
      regreet
    ]);

  # Regreet Configuration
  programs.regreet = {
    enable = true;
    settings = {
      background = {
        path = "/home/leon/Downloads/Wallpaper.jpg";
        fit = "Contain";
      };
      GTK = {
        application_prefer_dark_theme = true;
        cursor_theme_name             = "Bibata-Modern-Classic";
        font_name                     = "Iosevka 11";
        theme_name                    = "Orchis-Dark";
      };
      commands = {
        reboot = [ "systemctl" "reboot" ];
        poweroff = [ "systemctl" "poweroff" ];
      };
    };
  };

  # Greetd Configuration
  services.greetd = {
    enable = true;
    settings = rec {
      initial_session = let
        hyprlandGreetdConf = pkgs.writeText "hyprland-greetd-conf.conf" ''
          exec-once = ${lib.getExe pkgs.greetd.regreet}; hyprctl dispatch exit
          input {
            kb_layout = de
          }
          animations {
            enabled = false
            first_launch_animation = false
          }
          misc {
            disable_hyprland_logo = true
            disable_splash_rendering = true
          }
        '';
      in {
        command = "${lib.getExe inputs.hyprland.packages.${pkgs.system}.default} --config ${hyprlandGreetdConf}";
        user = "${config.custom.username}";
      };
      default_session = initial_session;
    };
  };
}
