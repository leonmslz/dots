let
  username = "nix";
  hostname = "nixos";


in
{

  username = username;
  hostname = hostname;

  homeDir = "/home/${username}";
}
