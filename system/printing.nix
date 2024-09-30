# printing.nix - Configuration File For Printing Service
{ pkgs, ... }:

{
  # Enable Printing Service
  services.printing.enable = true;

  # Enable Auto-Discovery Of Network Printers
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  # Printing Drivers
  services.printing.drivers = with pkgs; [ gutenprint brlaser ];
}
