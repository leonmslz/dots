{ inputs, config, pkgs, ... }:

{
  home.file.".config/emacs" = {
    source = ./.;
    recursive = true;
  };
  programs.emacs.enable = true;
}
