let
  username = "leon";
  hostname = "nixos";


in
{

  username = username;
  hostname = hostname;

  homeDir = "/home/${username}";
}
