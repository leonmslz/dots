{ config, ... }:

{
  programs.zsh = {
    enable = true;
    autocd = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    enableAutosuggestions = true;
    syntaxHighlighting.enable = true;

    shellAliases = {
      l          = "eza -alh --icons --group-directories-first";
      ls         = "eza -a --icons --group-directories-first";
      tree       = "eza --tree --icons --group-directories-first";
      nix-update = "sudo nixos-rebuild switch --flake ~/NixOS-System/.#default";
    };

    history = {
      size = 10000;
      path = "${config.xdg.dataHome}/zsh/history";
    };

    initExtra = ''
      export PROMPT="%F{cyan}( %F{yellow}  %F{green}$(basename $0) %F{red} %F{magenta}%~ %F{cyan}) %F{reset} "
    '';
  };
}
