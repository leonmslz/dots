# system.nix
#  _   _ _       ____   _____        _____             __ _
# | \ | (_)     / __ \ / ____|      / ____|           / _(_)
# |  \| |___  _| |  | | (___ ______| |     ___  _ __ | |_ _  __ _
# | . ` | \ \/ / |  | |\___ \______| |    / _ \| '_ \|  _| |/ _` |
# | |\  | |>  <| |__| |____) |     | |___| (_) | | | | | | | (_| |
# |_| \_|_/_/\_\\____/|_____/       \_____\___/|_| |_|_| |_|\__, |
#                                                            __/ |
#                                                           |___/
# Configuration File For NixOS

{ inputs, config, pkgs, unstablePkgs, ... }:

let
  inherit (import ../../globals.nix)
    username
    hostname
  ;
in
{
  imports = [
    ./hardware.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "${hostname}";

  networking.networkmanager.enable = true;

  time.timeZone = "Europe/Berlin";

  i18n.defaultLocale = "de_DE.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS        = "de_DE.UTF-8";
    LC_IDENTIFICATION = "de_DE.UTF-8";
    LC_MEASUREMENT    = "de_DE.UTF-8";
    LC_MONETARY       = "de_DE.UTF-8";
    LC_NAME           = "de_DE.UTF-8";
    LC_NUMERIC        = "de_DE.UTF-8";
    LC_PAPER          = "de_DE.UTF-8";
    LC_TELEPHONE      = "de_DE.UTF-8";
    LC_TIME           = "de_DE.UTF-8";
  };

  services.xserver.enable = true;
  services.xserver.libinput.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = true;

  services.xserver.xkb = {
    layout = "de";
    variant = "";
  };

  console.keyMap = "de";

  services.printing.enable = true;

  xdg.portal.enable = true;
  xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  users.users."${username}" = {
    isNormalUser = true;
    description = "leon";
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
   (with unstablePkgs; [
   ])
   ++
   (with pkgs; [
    htop
    pfetch
    eza
    tree
    ispell
    swww
    killall
    wl-clipboard
    zsh
    fish
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
    clojure
    go
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
