# default.nix (system/) - Include Configuration Files
{
  imports = [
    ./boot/grub.nix
    ./drivers/nvidia.nix
    ./display/sddm/sddm.nix
    ./bluetooth.nix
    ./locale.nix
    ./printing.nix
    ./audio.nix
    ./networking.nix
    ./stylix.nix
    ./steam.nix
  ];
}
