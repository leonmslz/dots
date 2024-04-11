{ inputs, config, pkgs, ... }:

{
  programs.firefox = {
    enable = true;

    profiles.leon = {

      search = {
        default = "Startpage";
        force = true;

        engines = {
          "Startpage" = {
            urls = [{
              template = "https://www.startpage.com/sp/search";
              params = [
                { name = "query"; value = "{searchTerms}"; }
              ];
            }];
          };

          "Nix Packages" = {
            urls = [{
              template = "https://search.nixos.org/packages";
              params = [
                { name = "type"; value = "packages"; }
                { name = "query"; value = "{searchTerms}"; }
              ];
            }];
            definedAliases = [ "!nix" ];
          };
        };
      };

    };
  };
}
