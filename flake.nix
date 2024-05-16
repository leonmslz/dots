# flake.nix - Flake For Custom NixOS Configuration
{
  description = "NixOS-Setup";

  # --- Inputs ---
  inputs = {
    nixpkgs.url                              = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url                         = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows      = "nixpkgs";
    nix-colors.url                           = "github:misterio77/nix-colors";
    # hyprland.url                             = "github:hyprwm/Hyprland";
    hyprland.url                             = "git+https://github.com/hyprwm/Hyprland?submodules=1";
    hyprland-plugins.url                     = "github:hyprwm/hyprland-plugins";
    hyprland-plugins.inputs.hyprland.follows = "hyprland";
  };

  # --- Outputs ---
  outputs = { self, nixpkgs, home-manager, hyprland, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      inherit (import ./globals.nix) username;

      myLib = import ./lib;
      inherit (myLib { inherit pkgs; scriptDir = (./home/scripts/bin); }) scripts;
      inherit (myLib { inherit inputs; }) mkSystem;
    in
      {
        # --- Systems ---
        nixosConfigurations = {

          # Desktop Computer
          default =
            mkSystem {
              username     = username;
              special-args = { inherit inputs scripts; };
              home-nix     = (./hosts/default/home.nix);
              system-nix   = (./hosts/default/system.nix);
            };

        };
      };
}
