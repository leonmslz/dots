# stylix.nix - Global Styling Settings/Options
{ pkgs, ... }:

{
  stylix = {
    enable = true;
    image = ./../assets/Wallpaper.png;

    # Gruvbox Dark Material Theme
    base16Scheme = {
      base00 = "242424"; # ----
      base01 = "45403d"; # ---
      base02 = "5a524c"; # --
      base03 = "7c6f64"; # -
      base04 = "a89984"; # +
      base05 = "c5b18d"; # ++
      base06 = "d4be98"; # +++
      base07 = "ddc7a1"; # ++++
      base08 = "ea6962"; # red
      base09 = "e78a4e"; # orange
      base0A = "d8a657"; # yellow
      base0B = "a9b665"; # green
      base0C = "89b482"; # aqua/cyan
      base0D = "7daea3"; # blue
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
