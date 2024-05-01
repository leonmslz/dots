{ inputs, config, pkgs, ... }:

let
  hyprplugins = inputs.hyprland-plugins.packages.${pkgs.system};

  wallpaperPath = "~/Downloads/Wallpaper.jpg";
in
{
  wayland.windowManager.hyprland = {
    enable = true;

    plugins = with hyprplugins; [
      hyprbars
    ];

    settings = with config.colorScheme.palette; {

      plugin = {
        hyprbars = {
          bar_height      = 20;
          bar_text_size   = 8;
          bar_text_font   = "Iosevka";
          bar_color       = "rgba(${base07}55)";
          "col.text"      = "rgba(${base00}FF)";
          hyprbars-button = "rgb(${base0E}), 12, , hyprctl dispatch killactive";
        };
      };

      input = {
        kb_layout = "de";
        kb_variant = "nodeadkeys";
        sensitivity = 6.0;
      };

      general = {
        gaps_in = 5;
        gaps_out = 10;
        border_size = 3;
        "col.active_border" = "rgba(${base07}FF)";
        "col.inactive_border" = "rgba(${base00}FF)";
        layout = "master";
      };

      decoration = {
        rounding = 5;
        drop_shadow = true;
        shadow_range = 4;
        shadow_render_power = 3;
        "col.shadow" = "rgba(${base00}EE)";
      };

      misc = {
        disable_hyprland_logo = true;
      };

      animations = {
        enabled = true;
        bezier = [
          "easeOutBack,0.34,1.56,0.64,1"
          "myBezier,0.05,0.9,0.1,1.05"
        ];
        animation = [
          "windows,1,7,myBezier"
          "windowsOut,1,7,default,popin 0%"
          "border,1,10,default"
          "fade,1,7,default"
          "workspaces,1,4,default,slidevert"
          "specialWorkspace,1,6,easeOutBack,slide"
        ];
      };

      master = {
        new_is_master = false;
        mfact         = "0.5";
      };

      windowrulev2 = [
        "float, class:.*"
      ];

      keybindings =
        let
          mainMod = "SUPER";
          workspaces = [1 2 3 4 5 6 7 8 9];
        in
          {
            bind =
              (map (x:
                "${mainMod}, ${(toString x)}, workspace, ${(toString x)}")
                workspaces)
              ++
              (map (x:
                "${mainMod} SHIFT, ${(toString x)}, movetoworkspace, ${(toString x)}")
                workspaces)
              ++
              [
                "${mainMod}, return, exec, alacritty"
                "${mainMod}, B, exec, firefox"
                "${mainMod}, C, killactive,"
                "${mainMod}, P, exec, pcmanfm"
                "${mainMod}, F, togglefloating,"
                "${mainMod}, space, exec, rofi -show drun"
                "${mainMod}, O, exec, hyprpicker | tr -d '\\n' | wl-copy"
                "${mainMod}, W, exec, grim -g \"$(slurp)\""
                "${mainMod}, E, exec, emacs"
                "${mainMod}, A, exec, amixer sset Master 5%-"
                "${mainMod}, D, exec, amixer sset Master 5%+"
                "${mainMod}, S, exec, amixer sset Master toggle"
                "${mainMod}, H, movefocus, l"
                "${mainMod}, J, movefocus, d"
                "${mainMod}, K, movefocus, u"
                "${mainMod}, L, movefocus, r"
                "${mainMod} CTRL, H, movewindow, l"
                "${mainMod} CTRL, J, movewindow, d"
                "${mainMod} CTRL, K, movewindow, u"
                "${mainMod} CTRL, L, movewindow, r"
                "${mainMod}, mouse_down, workspace, e-1"
                "${mainMod}, mouse_up, workspace, e+1"
              ];

            bindm =
              [
                "${mainMod}, mouse:272, movewindow"
                "${mainMod}, mouse:273, resizewindow"
              ];
          };

      exec-once = [
        "${pkgs.waybar}/bin/waybar &"
        "${pkgs.hyprpaper}/bin/hyprpaper"
        "${pkgs.networkmanagerapplet}/bin/nm-applet &"
      ];
    };
  };

  home.packages = with pkgs; [
    hyprpaper
  ];

  xdg.configFile."hypr/hyprpaper.conf".text = ''
    preload = ${wallpaperPath}
    wallpaper = ,${wallpaperPath}
    ipc=true
    splash=false
  '';
}
