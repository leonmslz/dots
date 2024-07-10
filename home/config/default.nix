# default.nix (home/config) - Include Configuration Files
{
  imports = [
    ./emacs
    ./zsh
    ./alacritty/alacritty.nix
    ./hyprland/hyprland.nix
    ./waybar/waybar.nix
    ./rofi/rofi.nix
    ./firefox/firefox.nix
    ./fastfetch/fastfetch.nix
  ];
}
