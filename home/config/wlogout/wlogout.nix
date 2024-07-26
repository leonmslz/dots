# wlogout.nix - Nix Declarative Configuration File For Wlogout Logout Menu
{ config, pkgs, ...}:

{
  programs.wlogout = {
    enable = true;
    package = pkgs.wlogout;

    layout = [
      {
        label   = "shutdown";
        action  = "sleep 1; systemctl poweroff";
        text    = "Shutdown";
        keybind = "s";
      }
      {
        label   = "reboot";
        action  = "sleep 1; systemctl reboot";
        text    = "Reboot";
        keybind = "r";
      }
      {
        label   = "logout";
        action  = "sleep 1; hyprctl dispatch exit";
        text    = "Exit";
        keybind = "e";
      }
    ];

    style = with config.lib.stylix.colors; ''
      @define-color background #${base00};
      @define-color foreground #${base07};
      @define-color border     #${base03};
      @define-color hover      #${base02};

      @import "${./style.css}";
    '';
  };
}
