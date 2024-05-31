{ ... }:

{
  # --- Grub ---

  boot.loader.efi.canTouchEfiVariables = true;

  # Enable Grub
  boot.loader.grub = {
    enable = true;
    useOSProber = true;
    devices = [ "nodev" ];
    efiSupport = true;
  };

}
