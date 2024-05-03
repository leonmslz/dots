inputs:
{
  inherit (import ./scripts.nix inputs) scripts;
  inherit (import ./mkSystem.nix inputs) mkSystem;
}
