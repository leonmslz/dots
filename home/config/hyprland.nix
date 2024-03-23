{ inputs, config, pkgs, ... }:

let
  hyprplugins = inputs.hyprland-plugins.packages.${pkgs.system};

  autostart = pkgs.pkgs.writeShellScriptBin "autostart" ''
      swww init
      waybar
  '';
in
{
  wayland.windowManager.hyprland = {
    enable = true;

    plugins = with hyprplugins; [
      hyprbars
    ];

    settings = {
      input = {
        kb_layout = "de";
        kb_variant = "nodeadkeys";
      };

      general = {
        gaps_in = 5;
        gaps_out = 10;
        border_size = 3;
        "col.active_border" = "rgba(ffffffff)";
        "col.inactive_border" = "rgba(000000ff)";
        layout = "master";
      };

      decoration = {
        rounding = 5;
        drop_shadow = true;
        shadow_range = 4;
        shadow_render_power = 3;
        "col.shadow" = "rgba(1a1a1aee)";
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

      plugin = {
        hyprbars = {
          bar_height      = 20;
          bar_text_size   = 8;
          bar_text_font   = "Iosevka";
          bar_color       = "rgba(FFFFFF55)";
          "col.text"      = "rgba(0a0908FF)";
          hyprbars-button = "rgb(D97A7A), 12, , hyprctl dispatch killactive";
        };
      };

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

      exec-once = "${autostart}/bin/autostart";
    };
  };
}
