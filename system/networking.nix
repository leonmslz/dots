# networking.nix - Configuration File For Networking Setup
{ config, ... }:

{
  networking.hostName = "${config.custom.hostname}";

  networking.networkmanager.enable = true;
}
