# grub.nix - Configuration File For Grub-Bootloader
{ pkgs, ... }:

let
  grub-gruvbox-theme = pkgs.fetchFromGitHub {
    owner  = "AllJavi";
    repo   = "tartarus-grub";
    rev    = "b116360a2a0991062a4d728cb005dfd309fbb82a";
    sha256 = "0igy1aqp3v2v8gqqlk0p5i78g9l3xz19fh3aigafbk7k3p8ypz7w";
  } + "/tartarus";
in
{
  # --- Grub ---

  boot.loader.efi.canTouchEfiVariables = true;

  # Enable Grub
  boot.loader.grub = {
    enable      = true;
    theme       = grub-gruvbox-theme;
    useOSProber = true;
    devices     = [ "nodev" ];
    efiSupport  = true;
  };

}
