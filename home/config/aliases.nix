let
  inherit (import ../../globals.nix)
    flakeDir
  ;
in
{
  l           = "eza --icons -alh --group-directories-first";
  ls          = "eza -a --icons --group-directories-first";
  tree        = "eza --tree --icons --group-directories-first";
  nix-rebuild = "sudo nixos-rebuild switch --flake ${flakeDir}#default";
  nix-clean   = "sudo nix-collect-garbage --delete-older-than 1d";
}
