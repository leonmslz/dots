# rofi.nix - Nix Declarative Configuration File For Rofi Run Launcher
{ inputs, config, pkgs, lib, ... }:

let
  inherit (config.lib.formats.rasi) mkLiteral;

  rofiTheme = {

    "*" = with config.lib.stylix.colors; {
      font                 = "ZedMono NF, FontAwesome 10.5";

      b-color              = mkLiteral "#${base00}FF";
      wbg-color            = mkLiteral "#${base00}BB";
      bb-color             = mkLiteral "#${base03}FF";
      fg-color             = mkLiteral "#${base07}FF";
      fgp-color            = mkLiteral "#${base04}FF";
      hl-color             = mkLiteral "#${base0D}FF";
      hl2-color            = mkLiteral "#${base0A}FF";
      w-border-color       = mkLiteral "#${base03}FF";

      g-spacing            = mkLiteral "10px";
      g-margin             = mkLiteral "0";
      b-radius             = mkLiteral "8px";
      g-padding            = mkLiteral "8px";
      w-padding            = mkLiteral "12px";
      w-border             = mkLiteral "2px solid";

      text-color           = mkLiteral "@fg-color";
      background-color     = mkLiteral "transparent";
    };

    "listview"             = {
      columns              = 1;
      lines                = 7;
      fixed-height         = true;
      fixed-columns        = true;
      cycle                = false;
      scrollbar            = false;
      border               = mkLiteral "0px solid";
    };

    "window"               = {
      transparency         = "real";
      width                = mkLiteral "450px";
      border-radius        = mkLiteral "@b-radius";
      background-color     = mkLiteral "@wbg-color";
      border               = mkLiteral "@w-border";
      border-color         = mkLiteral "@w-border-color";
      padding              = mkLiteral "@w-padding";
    };

    "inputbar"             = {
      children             = map mkLiteral ["textbox-prompt-colon" "entry" "mode-switcher"];
      padding              = mkLiteral "10px 0px";
      spacing              = mkLiteral "@g-spacing";
    };

    "textbox-prompt-colon" = {
      enabled              = true;
      expand               = false;
      str                  = " ";
      padding              = mkLiteral "5px 8px";
      border-radius        = mkLiteral "@b-radius";
      background-color     = mkLiteral "@hl2-color";
      text-color           = mkLiteral "inherit";
    };

    "entry"                = {
      placeholder          = "Search...";
      padding              = mkLiteral "5px 8px";
      placeholder-color    = mkLiteral "@fgp-color";
      border-radius        = mkLiteral "@b-radius";
      background-color     = mkLiteral "@bb-color";
      text-color           = mkLiteral "inherit";
    };

    "mainbox"              = {
      spacing              = mkLiteral "@g-spacing";
      margin               = mkLiteral "@g-margin";
      padding              = mkLiteral "@g-padding";
      children             = map mkLiteral ["inputbar" "message" "listview"];
    };

    "element"              = {
      spacing              = mkLiteral "@g-spacing";
      margin               = mkLiteral "@g-margin";
      padding              = mkLiteral "@g-padding";
      border               = mkLiteral "0px solid";
      border-radius        = mkLiteral "@b-radius";
      border-color         = mkLiteral "@b-color";
    };

    "element selected"     = {
      text-color           = mkLiteral "@fgp-color";
      background-color     = mkLiteral "@hl-color";
    };

    "mode-switcher"        = {
      enabled              = true;
      spacing              = mkLiteral "5px";
      margin               = mkLiteral "0px";
      padding              = mkLiteral "0px";
      border               = mkLiteral "0px solid";
      border-radius        = mkLiteral "0px";
      border-color         = mkLiteral "@w-border-color";
      background-color     = mkLiteral "transparent";
      text-color           = mkLiteral "@fg-color";
    };

    "button"               = {
      padding              = mkLiteral "5px 8px";
      border               = mkLiteral "0px solid";
      border-radius        = mkLiteral "@b-radius";
      border-color         = mkLiteral "@w-border-color";
      background-color     = mkLiteral "@wbg-color";
      text-color           = mkLiteral "inherit";
      cursor               = mkLiteral "pointer";
    };

    "button selected"      = {
      background-color     = mkLiteral "@hl2-color";
      text-color           = mkLiteral "@wbg-color";
    };
  };
in
{
  programs.rofi = {
    enable = true;

    package = pkgs.rofi-wayland.override {
      plugins = with pkgs; [
        (rofi-emoji.override { rofi-unwrapped = rofi-wayland-unwrapped; })
      ];
    };

    extraConfig = {
      modes = [
        "drun"
        "emoji"
        "filebrowser"
      ];

      show-icons = true;

      display-drun        = " ";
      display-emoji       = " ";
      display-filebrowser = " ";
    };

    theme = rofiTheme;
  };
}
