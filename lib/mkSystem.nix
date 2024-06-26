# mkSystem.nix - Function For Creating A New NixOS-System
{ inputs, ... }:

let

  # --- Functions ---
  # Function For Creating A New NixOS-System
  mkSystem = { username, home-nix, system-nix, special-args }:
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = special-args;
      modules = [
        system-nix
        inputs.stylix.nixosModules.stylix
        inputs.home-manager.nixosModules.home-manager {
          home-manager.extraSpecialArgs = special-args;
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users."${username}" = import home-nix;
        }
      ];
    };

in
{
  # --- Export ---
  mkSystem = mkSystem;
}
