# scripts.nix - Module For Creating Script-Derivations Out Of Shell-Scripts
{ pkgs, scriptDir, ... }:

let

  # --- Variables ---
  baseDir         = scriptDir;
  baseDirAbsolute = builtins.toPath baseDir;
  shellShebang    = "#!/usr/bin/env bash";

  # --- Helper Functions ---
  # Dropping File-Extension (e.g. "test.sh" -> "test")
  dropShellExtension = fileName:
    builtins.head (builtins.split ".sh" fileName);

  # Creating Derivation Out Of A Shell Script
  createScriptDerivation = script:
    pkgs.writeScriptBin (dropShellExtension script) ''
        ${shellShebang}
        source ${baseDirAbsolute}/${script}
      '';

  # Combining Script-Name And Derivation To A Set
  getScript = script: {
    name  = dropShellExtension script;
    value = createScriptDerivation script;
  };

  # Collecting All Script-Derivations In A Set For Easier Access (e.g. scripts.test -> test.drv)
  scripts = builtins.listToAttrs
    (map getScript
      (builtins.attrNames (builtins.readDir baseDir)));

in
{
  # --- Export ---
  scripts = scripts;
}
