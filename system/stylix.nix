# stylix.nix - Global Styling Settings/Options
{ pkgs, ... }:

{
  stylix = {
    enable = true;
    image = ./../assets/Wallpaper.png;

    # Gruvbox Dark Medium Theme
    # Reference: <https://github.com/tinted-theming/base16-schemes/blob/main/gruvbox-dark-medium.yaml>
    base16Scheme = {
      base00 = "282828"; # ----
      base01 = "3c3836"; # ---
      base02 = "504945"; # --
      base03 = "665c54"; # -
      base04 = "bdae93"; # +
      base05 = "d5c4a1"; # ++
      base06 = "ebdbb2"; # +++
      base07 = "fbf1c7"; # ++++
      base08 = "fb4934"; # red
      base09 = "fe8019"; # orange
      base0A = "fabd2f"; # yellow
      base0B = "b8bb26"; # green
      base0C = "8ec07c"; # aqua/cyan
      base0D = "83a598"; # blue
      base0E = "d3869b"; # purple
      base0F = "d65d0e"; # brown
    };

    autoEnable = false;

    cursor = {
      package = pkgs.bibata-cursors;
      name    = "Bibata-Modern-Classic";
      size    = 24;
    };

    fonts = {
      emoji = {
        name    = "Noto Color Emoji";
        package = pkgs.noto-fonts-emoji;
      };
      monospace = {
        name    = "ZedMono Nerd Font";
        package = pkgs.iosevka-bin;
      };
    };
  };

  fonts.packages = with pkgs; [
    iosevka-bin
    fira-code
    ubuntu_font_family
    inconsolata
    nerdfonts
    font-awesome
    noto-fonts-emoji
  ];
}
