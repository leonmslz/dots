# bluetooth.nix - Configuration File For Bluetooth Support
{ config, ... }:

{
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  services.blueman.enable = true;
}
