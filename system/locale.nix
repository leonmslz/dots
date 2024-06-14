# locale.nix - Configuration File For Locale Settings
{ config, ... }:

let
  timeZone = "Europe/Berlin";
  locale   = "de_DE.UTF-8";
in
{
  time.timeZone = "${timeZone}";

  i18n.defaultLocale = "${locale}";

  i18n.extraLocaleSettings = {
    LC_ADDRESS        = "${locale}";
    LC_IDENTIFICATION = "${locale}";
    LC_MEASUREMENT    = "${locale}";
    LC_MONETARY       = "${locale}";
    LC_NAME           = "${locale}";
    LC_NUMERIC        = "${locale}";
    LC_PAPER          = "${locale}";
    LC_TELEPHONE      = "${locale}";
    LC_TIME           = "${locale}";
  };
}
