{ pkgs, ... }:

with pkgs;

let
  projectName = "python";
  pythonPackages = ps:
    with ps;
    [
      (buildPythonPackage rec {
        pname = "databases";
        version = "0.8.0";
        src = fetchPypi {
          inherit pname version;
          sha256 = "sha256-ZUTYLpkm8jPWlOwpzQGEA0RMf7boY6+IGoME0f9c+5A=";
        };
        doCheck = false;
        propagatedBuildInputs = [
          (buildPythonPackage rec {
            pname = "SQLAlchemy";
            version = "1.4.49";
            src = fetchPypi {
              inherit pname version;
              sha256 = "sha256-Bv8ly64ww5bEt3N0ZPKn/Deme32kCZk7GCsCTOyArtk=";
            };
            doCheck = false;
            propagatedBuildInputs = [ pkgs.python311Packages.greenlet ];
          })
        ];
      })
    ];
in mkShell {
  name = "${projectName}-shell";

  packages = [ (python3.withPackages pythonPackages) ];
}
