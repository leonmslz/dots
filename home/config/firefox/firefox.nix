# firefox.nix - Nix Declarative Configuration File For Firefox Web Browser
{ config, pkgs, ... }:

{
  programs.firefox = {
    enable = true;
    package = pkgs.firefox;

    profiles.leon = {

      extensions = with config.nur.repos.rycee.firefox-addons; [
        ublock-origin
        proton-pass
        return-youtube-dislikes
        betterttv
        gruvbox-dark-theme
      ];

      settings = {
        "intl.locale.requested" = "de,en-US";
      };

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

          "Wikipedia" = {
            urls = [{
              template = "https://de.wikipedia.org/w/index.php";
              params = [
                { name = "title"; value = "Special:Search"; }
                { name = "search"; value = "{searchTerms}"; }
              ];
            }];
            definedAliases = [ "!wiki" ];
          };
        };
      };
    };
  };
}
