{ inputs, config, pkgs, ... }:

{
  programs.alacritty = {
    enable = true;

    settings = {

      colors = {
        primary = {
          background = "#2d353b";
          foreground = "#d3c6aa";
        };
        bright = {
          black    = "#475258";
          blue     = "#7fbbb3";
          cyan     = "#83c092";
          green    = "#a7c080";
          magenta  = "#d699b6";
          red      = "#e67e80";
          white    = "#d3c6aa";
          yellow   = "#dbbc7f";
        };
        normal = {
          black    = "#475258";
          blue     = "#7fbbb3";
          cyan     = "#83c092";
          green    = "#a7c080";
          magenta  = "#d699b6";
          red      = "#e67e80";
          white    = "#d3c6aa";
          yellow   = "#dbbc7f";
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
