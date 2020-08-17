let
  pkgs = import
    (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") { };
  py = pkgs.python38Full.withPackages (p: [ p.flask ]);
in pkgs.stdenv.mkDerivation {
  name = "api_tiny_exposition";
  buildInputs = [ py ];
  shellHooks = "export FLASK_APP=app.py";
}
