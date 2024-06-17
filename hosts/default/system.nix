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

  fonts.packages = with pkgs; [
    iosevka-bin
    inconsolata
    nerdfonts
    font-awesome
    noto-fonts-emoji
  ];

  system.stateVersion = "23.11";
}
