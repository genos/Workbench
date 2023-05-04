let
  tgz = "https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz";
  pkgs = import (fetchTarball tgz) { };
  py = pkgs.python36.withPackages (p: [ p.hypothesis p.pytest ]);
in pkgs.stdenv.mkDerivation {
  name = "ttkv";
  buildInputs = [ py ];
  shellHook = ''
    it () {
      pytest ttkv_spec.py
    }
  '';
}
