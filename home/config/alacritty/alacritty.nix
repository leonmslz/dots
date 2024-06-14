# alacritty.nix - Nix Declarative Configuration File For Alacritty Terminal Emulator
{ config, ... }:

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
          black    = "#${base02}";
          blue     = "#${base0D}";
          cyan     = "#${base0C}";
          green    = "#${base0B}";
          magenta  = "#${base0E}";
          red      = "#${base08}";
          white    = "#${base07}";
          yellow   = "#${base0A}";
        };
        normal = {
          black    = "#${base02}";
          blue     = "#${base0D}";
          cyan     = "#${base0C}";
          green    = "#${base0B}";
          magenta  = "#${base0E}";
          red      = "#${base08}";
          white    = "#${base07}";
          yellow   = "#${base0A}";
        };
        draw_bold_text_with_bright_colors = true;
      };

      window = {
        opacity = 0.75;
        decorations = "none";
        padding = {
          x = 3;
          y = 3;
        };
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
