# flake.nix - Flake For Custom NixOS Configuration
{
  description = "NixOS-Setup";

  # --- Inputs ---
  inputs = {
    nixpkgs.url                              = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url                         = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows      = "nixpkgs";
    nix-colors.url                           = "github:misterio77/nix-colors";
    hyprland.url                             = "github:hyprwm/Hyprland";
    hyprland-plugins.url                     = "github:hyprwm/hyprland-plugins";
    hyprland-plugins.inputs.hyprland.follows = "hyprland";
  };

  # --- Outputs ---
  outputs = { self, nixpkgs, home-manager, hyprland, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      inherit (import ./globals.nix) username;
      inherit (import ./lib/scripts.nix { pkgs = (pkgs); scriptDir = (./home/scripts/bin); }) scripts;
    in
      {
        # --- Systems ---
        nixosConfigurations = {

          default = # Desktop Computer
            nixpkgs.lib.nixosSystem {
              specialArgs = { inherit inputs scripts; };
              modules = [
                ./hosts/default/system.nix
                inputs.home-manager.nixosModules.home-manager {
                  home-manager.extraSpecialArgs = { inherit inputs scripts; };
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.users."${username}" = import ./hosts/default/home.nix;
                }
              ];
            };

        };
      };
}
