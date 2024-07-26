# fastfetch.nix - Nix Declarative Configuration File For Fastfetch Utility
{ config, pkgs, ... }:

{
  programs.fastfetch = {
    enable = true;
    package = pkgs.fastfetch;

    settings = {
      "logo" = "nixos";

      "display" = {
        "separator" = "   ";
      };

      modules = [
        "break"
        {
          type        = "custom";
          format      = "                              ";
          outputColor = "90";
        }
        "break"
        {
          type        = "title";
          format      = "{1}@{2}";
          key         = " ";
          keyColor    = "red";
        }
        "break"
        {
          type        = "os";
          key         = " ";
          keyColor    = "green";
        }
        {
          type        = "packages";
          key         = " ";
          keyColor    = "green";
        }
        {
          type        = "shell";
          key         = " ";
          keyColor    = "green";
        }
        "break"
        {
          type        = "wm";
          key         = " ";
          keyColor    = "yellow";
        }
        "break"
        {
          type        = "host";
          key         = " ";
          keyColor    = "blue";
        }
        {
          type        = "cpu";
          key         = " ";
          keyColor    = "blue";
        }
        {
          type        = "gpu";
          key         = " ";
          keyColor    = "blue";
        }
        "break"
        {
          type        = "disk";
          key         = " ";
          keyColor    = "magenta";
        }
        {
          type        = "memory";
          key         = "󰑭 ";
          keyColor    = "magenta";
        }
        "break"
        {
          type        = "uptime";
          key         = " ";
          keyColor    = "cyan";
        }
        "break"
        {
          type        = "custom";
          format      = "                              ";
          outputColor = "90";
        }
        "break"
      ];
    };
  };
}
