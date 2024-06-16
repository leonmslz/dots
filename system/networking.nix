# networking.nix - Configuration File For Networking Setup
{ config, o, ... }:

{
  networking.hostName = "${o.hostname}";

  networking.networkmanager.enable = true;
}
