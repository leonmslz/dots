{ config, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    autocd = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;

    shellAliases = import ./aliases.nix;

    history = {
      size = 10000;
      path = "${config.xdg.dataHome}/zsh/history";
    };

    initExtra = ''
      export PROMPT="%F{cyan}( %F{yellow}  %F{green}$(basename $0) %F{red} %F{magenta}%~ %F{cyan}) %F{reset} "

      pfetch
    '';
  };
}
