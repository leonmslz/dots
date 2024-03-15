{
  description = "Nixos config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland.url = "github:hyprwm/Hyprland";

    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };
  };

  outputs = { self, nixpkgs, home-manager, hyprland, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (import ./globals.nix)
        username
      ;
    in
      {
        nixosConfigurations.default = nixpkgs.lib.nixosSystem {
          specialArgs = {inherit inputs;};
          modules = [
            ./hosts/default/system.nix
            inputs.home-manager.nixosModules.home-manager {
              home-manager.extraSpecialArgs = { inherit inputs; };
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users."${username}" = import ./hosts/default/home.nix;
            }
          ];
        };

      };
}
