# hyprland.nix - Nix Declarative Configuration File For Hyprland Wayland Compositor
{ inputs, config, lib, pkgs, ... }:

let
  # Hyprland Plugins
  hyprplugins = inputs.hyprland-plugins.packages.${pkgs.system};
in
{
  # --- Hyprland ---
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    systemd.enable = true;

    # Hyprland plugins
    plugins = with hyprplugins; [
      hyprbars hyprexpo
    ];

    settings = with config.lib.stylix.colors; {

      # --- Plugins ---
      plugin = {

        # Hyprexpo - Overview Plugin
        hyprexpo = {
          columns = 3;
          gap_size = 5;
          bg_col = "rgb(111111)";
          workspace_method = "first 1";
        };

        # Hyprbars - Title Bar Plugin
        hyprbars = {
          bar_height      = 20;
          bar_text_size   = 8;
          bar_text_font   = "ZedMono Nerd Font";
          bar_color       = "rgba(${base00}BB)";
          "col.text"      = "rgba(${base07}FF)";
          hyprbars-button = [
            "rgb(${base08}), 10, , hyprctl dispatch killactive"
            "rgb(${base0B}), 10, , hyprctl dispatch togglefloating"
            "rgb(${base0A}), 10, , hyprctl dispatch fullscreen"
          ];
        };

      };

      input = {
        kb_layout = "de";
        kb_variant = "nodeadkeys";
        sensitivity = 6.0;
      };

      general = {
        gaps_in = 7.5;
        gaps_out = 15;
        border_size = 2;
        "col.active_border" = "rgba(${base03}FF)";
        "col.inactive_border" = "rgba(${base00}FF)";
        layout = "master";
      };

      decoration = {
        rounding = 10;
        drop_shadow = true;
        shadow_range = 10;
        shadow_render_power = 5;
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
        first_launch_animation = true;
      };

      master = {
        # new_is_master = false;
        mfact         = "0.5";
      };

      # --- Window Rules ---
      windowrulev2 = [
        # Make All Windows Floating By Default
        "float, class:.*"

        # Except ...
        "maximize, class:^(firefox)$"
        "tile, class:^(emacs)$"
        "tile, class:^(Alacritty)$"
        "maximize, class:^(Cider)$"

        # Size Adjustments
        "size 600 600, class:^(org.gnome.Nautilus)$"
      ];

      # --- Keybindings ---
      _ =
        let
          mainMod    = "SUPER";
          workspaces = [1 2 3 4 5 6 7 8 9];
        in
          {
            bind = lib.flatten
              [
                # Switch Workspace
                (map (x:
                  "${mainMod}, ${(toString x)}, workspace, ${(toString x)}")
                  workspaces)

                "${mainMod}, mouse_down, workspace, e-1"
                "${mainMod}, mouse_up,   workspace, e+1"

                # Move Application To Workspace
                (map (x:
                  "${mainMod} SHIFT, ${(toString x)}, movetoworkspace, ${(toString x)}")
                  workspaces)

                # Open Applications
                "${mainMod}, return, exec, alacritty"
                "${mainMod}, B,      exec, firefox"
                "${mainMod}, P,      exec, pcmanfm"
                "${mainMod}, E,      exec, emacs"

                # Kill Active Clint
                "${mainMod}, C, killactive,"

                # Toggle Floating On Active Client
                "${mainMod}, F, togglefloating,"

                # Color Picker Tool
                "${mainMod}, O, exec, hyprpicker | tr -d '\\n' | wl-copy"

                # Make Screenshot
                "${mainMod}, W, exec, slurp | grim -g - $HOME/screenshot.png"

                # Change Volume
                "${mainMod}, A, exec, amixer sset Master 5%-"
                "${mainMod}, D, exec, amixer sset Master 5%+"
                "${mainMod}, S, exec, amixer sset Master toggle"

                # Move Focus
                "${mainMod}, H, movefocus, l"
                "${mainMod}, J, movefocus, d"
                "${mainMod}, K, movefocus, u"
                "${mainMod}, L, movefocus, r"

                # Move Window
                "${mainMod} CTRL, H, movewindow, l"
                "${mainMod} CTRL, J, movewindow, d"
                "${mainMod} CTRL, K, movewindow, u"
                "${mainMod} CTRL, L, movewindow, r"

                # Hyprexpo Plugin
                "${mainMod}, p, hyprexpo:expo, toggle"
              ];

            bindr = lib.flatten
              [
                # Open Program Launcher
                "${mainMod}, Super_L, exec, rofi -show drun"
                "${mainMod}, Super_R, exec, rofi -show drun"
              ];

            bindm = lib.flatten
              [
                # Move Active Window
                "${mainMod}, mouse:272, movewindow"
                # Resize Active Window
                "${mainMod}, mouse:273, resizewindow"
              ];
          };

      # --- Troubleshooting ---
      # Issue due to Hyprland (v0.40.0) and Nvidia-Drivers
      monitor = [ "Unknown-1,disable" ];

      # Issue with explicit Sync in Hyprland (v0.42.0) due to Nvidia-Drivers
      # <https://hyprland.org/news/update42/>
      render = {
        explicit_sync = false;
      };

      # --- Autostart ---
      exec-once = [
        # "${pkgs.waybar}/bin/waybar &"                  # Waybar Status-Bar
        "${pkgs.eww}/bin/eww daemon &"                 # Eww Daemon
        "${pkgs.eww}/bin/eww open bar"                 # Eww Status-Bar
        "${pkgs.hyprpaper}/bin/hyprpaper &"            # Hyprpaper Wallpaper Utility
        "${pkgs.networkmanagerapplet}/bin/nm-applet &" # Network-Manager Applet
        "${pkgs.blueman}/bin/blueman-applet &"         # Bluetooth-Manager Applet
      ];
    };
  };
}
