let
  pkgs = import (
    fetchTarball https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz
  ) {};
in
  pkgs.stdenv.mkDerivation {
    name = "ttkv";
    buildInputs = [ pkgs.sbt ];
    shellHook = ''
      it () {
        ${pkgs.sbt}/bin/sbt test
      }
    '';
  }
