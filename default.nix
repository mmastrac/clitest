{ rustPlatform, lib }:

rustPlatform.buildRustPackage {
  pname = "clitest";
  version = "0.6.0";

  src = ./.;

  cargoLock = {
    lockFile = ./Cargo.lock;
  };

  cargoBuildFlags = [
    "-p"
    "clitest"
  ];
  cargoTestFlags = [
    "-p"
    "clitest"
  ];

  meta = with lib; {
    description = "CLI/test: A literate CLI testing tool";
    homepage = "https://github.com/mmastrac/clitest";
    license = [
      licenses.mit
      licenses.asl20
    ];
  };
}
