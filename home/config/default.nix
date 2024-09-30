# default.nix (home/config) - Include Configuration Files
{
  imports = [
    ./emacs
    ./eww
    ./zsh
    ./alacritty/alacritty.nix
    ./hypr/hyprland.nix
    ./hypr/hyprpaper.nix
    ./waybar/waybar.nix
    ./rofi/rofi.nix
    ./firefox/firefox.nix
    ./fastfetch/fastfetch.nix
    ./wlogout/wlogout.nix
  ];
}
