{ inputs, config, pkgs, ... }:

{
  programs.alacritty = {
    enable = true;

    settings = {

      colors = with config.colorScheme.palette; {
        primary = {
          background = "#${base00}";
          foreground = "#${base07}";
        };
        bright = {
          black    = "#475258";
          blue     = "#${base08}";
          cyan     = "#${base0B}";
          green    = "#${base0D}";
          magenta  = "#${base09}";
          red      = "#${base0E}";
          white    = "#${base07}";
          yellow   = "#${base0A}";
        };
        normal = {
          black    = "#475258";
          blue     = "#${base08}";
          cyan     = "#${base0B}";
          green    = "#${base0D}";
          magenta  = "#${base09}";
          red      = "#${base0E}";
          white    = "#${base07}";
          yellow   = "#${base0A}";
        };
        draw_bold_text_with_bright_colors = true;
      };

      window.padding = {
        x = 3;
        y = 3;
      };

      font = {
        size = 10.5;
        bold = {
          family   = "Iosevka";
          style    = "Bold";
        };
        italic =  {
          family   = "Iosevka";
          style    = "Italic";
        };
        normal = {
          family   = "Iosevka";
          style    = "Regular";
        };
      };

      keyboard.bindings = [
        {
          action  = "ResetFontSize";
          key     = "R";
          mods    = "Control|Shift";
        }
      ];
    };
  };
}
