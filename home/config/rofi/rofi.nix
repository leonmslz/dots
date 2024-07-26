# rofi.nix - Nix Declarative Configuration File For Rofi Run Launcher
{ inputs, config, pkgs, ... }:

let
  inherit (config.lib.formats.rasi) mkLiteral;

  rofiTheme = {

    "*" = with config.lib.stylix.colors; {
      font           = "FontAwesome, ZedMono Nerd Font 12";

      b-color        = mkLiteral "#${base00}FF";
      wbg-color      = mkLiteral "#${base00}DD";
      fg-color       = mkLiteral "#${base07}FF";
      fgp-color      = mkLiteral "#${base04}FF";
      hl-color       = mkLiteral "#${base0D}FF";
      w-border-color = mkLiteral "#${base03}FF";

      g-spacing      = mkLiteral "10px";
      g-margin       = mkLiteral "0";
      b-radius       = mkLiteral "8px";
      g-padding      = mkLiteral "8px";
      w-padding      = mkLiteral "12px";
      w-border       = mkLiteral "2px solid";

      text-color = mkLiteral "@fg-color";
      background-color = mkLiteral "transparent";
    };

    "listview" = {
      columns = 1;
      lines = 7;
      fixed-height = true;
      fixed-columns = true;
      cycle = false;
      scrollbar = false;
      border = mkLiteral "0px solid";
    };

    "window" = {
      transparency     = "real";
      width            = mkLiteral "450px";
      border-radius    = mkLiteral "@b-radius";
      background-color = mkLiteral "@wbg-color";
      border           = mkLiteral "@w-border";
      border-color     = mkLiteral "@w-border-color";
      padding          = mkLiteral "@w-padding";
    };

    "inputbar" = {
      children = map mkLiteral ["prompt" "entry"];
      spacing = mkLiteral "@g-spacing";
    };

    "entry" = {
      placeholder       = "Search Apps";
      placeholder-color = mkLiteral "@fgp-color";
    };

    "mainbox" = {
      spacing  = mkLiteral "@g-spacing";
      margin   = mkLiteral "@g-margin";
      padding  = mkLiteral "@g-padding";
      children = map mkLiteral ["inputbar" "message" "listview"];
    };

    "element" = {
      spacing          = mkLiteral "@g-spacing";
      margin           = mkLiteral "@g-margin";
      padding          = mkLiteral "@g-padding";
      border           = mkLiteral "0px solid";
      border-radius    = mkLiteral "@b-radius";
      border-color     = mkLiteral "@b-color";
    };

    "element alternate" = {
      background-color = mkLiteral "@b-color";
    };

    "element selected" = {
      text-color       = mkLiteral "@fgp-color";
      background-color = mkLiteral "@hl-color";
    };
  };
in
{
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;

    extraConfig = {
      modi = "drun";
      show-icons = true;
      display-drun = "";
    };

    theme = rofiTheme;
  };
}
