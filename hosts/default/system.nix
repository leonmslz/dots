# system.nix - Base Configuration File For NixOS-System
{ inputs, config, pkgs, o, ... }:

{
  imports = [
    ./hardware.nix
    ./../../system
  ];

  services.xserver.enable = true;
  services.libinput.enable = true;

  services.xserver.xkb = {
    layout = "de";
    variant = "";
  };

  console.keyMap = "de";

  xdg.portal.enable = true;
  xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  users.users."${o.username}" = {
    isNormalUser = true;
    description = "";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
    ];
  };

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  stylix = {
    enable = true;
    image = ./../../assets/Wallpaper.png;

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
        name    = "Iosevka";
        package = pkgs.iosevka-bin;
      };
    };
  };

  fonts.packages = with pkgs; [
    iosevka-bin
    inconsolata
    nerdfonts
    font-awesome
    noto-fonts-emoji
  ];

  programs.hyprland.enable = true;
  programs.hyprland.package = inputs.hyprland.packages."${pkgs.system}".hyprland;
  programs.hyprland.xwayland.enable = true;

  nixpkgs.config.allowUnfree = true;

  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = false;

  services.openssh.enable = true;

  environment.systemPackages =
    (with pkgs; [
      htop
      pfetch
      eza
      tree
      ispell
      killall
      wl-clipboard
      zsh
      git
      gnupg
      openssh
      pkg-config
      gcc
      ghc
      gnumake
      cmake
      meson
      cpio
      ninja
      cairo
      pango
      glib
      go
      libtool
      sbcl
      catppuccin-sddm-corners
      bibata-cursors
      lxappearance

      slurp
      grim
    ]);

  system.stateVersion = "23.11";
}
