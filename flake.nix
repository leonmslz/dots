# flake.nix - Flake For Custom NixOS Configuration
{
  description = "NixOS-Setup";

  # --- Inputs ---
  inputs = {
    # Nix-Packages Unstable Branch
    nixpkgs.url                              = "github:nixos/nixpkgs/nixos-unstable";
    # Home-Manger
    home-manager.url                         = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows      = "nixpkgs";
    # Hyprland Wayland Compositor
    hyprland.url                             = "git+https://github.com/hyprwm/Hyprland?submodules=1";
    hyprland-plugins.url                     = "github:hyprwm/hyprland-plugins";
    hyprland-plugins.inputs.hyprland.follows = "hyprland";
    # Stylix
    stylix.url                               = "github:danth/stylix";
    # Nix User Repository
    nur.url                                  = "github:nix-community/NUR";
  };

  # --- Outputs ---
  outputs = { self, nixpkgs, home-manager, hyprland, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      # -- Global Options ---
      o = rec {
        username = "leon";
        hostname = "nixos";
        homeDir  = "/home/${username}";
        flakeDir = "${homeDir}/NixOS-System";
      };

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
              username     = o.username;
              special-args = { inherit inputs scripts o; };
              home-nix     = (./hosts/default/home.nix);
              system-nix   = (./hosts/default/system.nix);
            };

        };
      };
}
