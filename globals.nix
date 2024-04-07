let
  username = "leon";
  hostname = "nixos";

  homeDir = "/home/${username}";
in
{
  username = username;
  hostname = hostname;

  homeDir = homeDir;

  flakeDir = "${homeDir}/NixOS-System/.";
}
