# grub.nix - Configuration File For Grub-Bootloader
{ ... }:

{
  # --- Grub ---

  boot.loader.efi.canTouchEfiVariables = true;

  # Enable Grub
  boot.loader.grub = {
    enable      = true;
    useOSProber = true;
    devices     = [ "nodev" ];
    efiSupport  = true;
  };

}
