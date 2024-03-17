{ pkgs, enableExercism, ... }:

with pkgs;

let
  projectName = "my-project";

  exercismPython = p:
    with p; [
      pytest
      pytest-cache
      pytest-subtests
      pytest-pylint
    ];

  fSharp = [ exercism dotnet-sdk_7 dotnet-runtime_7 ];
in mkShell {
  name = "${projectName}-shell";

  packages = [ go gopls gomodifytags gotests gore gotools ]
    ++ (if enableExercism then
      [ exercism (python3.withPackages exercismPython) ] ++ fSharp
    else
      [ ]);

  GOROOT = "${pkgs.go}/share/go";
}
