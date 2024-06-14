# options.nix - Declaration Of Global Options
{ lib, ... }:

{
  options = {
    custom.username = lib.mkOption {
      type = lib.types.nonEmptyStr;
      default = "user";
    };
    custom.hostname = lib.mkOption {
      type = lib.types.nonEmptyStr;
      default = "nixos";
    };
  };

  config = {
    custom.username = lib.mkForce "leon";
  };
}
