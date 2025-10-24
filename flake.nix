{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    wild-linker-overlay = {
      url = "github:davidlattimore/wild";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      nixpkgs,
      rust-overlay,
      wild-linker-overlay,
      ...
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forEachSystem =
        f:
        nixpkgs.lib.genAttrs supportedSystems (
          system:
          let
            overlays = [
              (import rust-overlay)
              (import wild-linker-overlay)
            ];

            pkgs = import nixpkgs {
              inherit system overlays;
            };

            rust = pkgs.pkgsBuildHost.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;

            wild-linker = pkgs.useWildLinker pkgs.stdenv;

            inputs = [
              rust
            ];
          in
          f {
            inherit
              pkgs
              rust
              wild-linker
              inputs
              ;
          }
        );
    in
    {
      devShells = forEachSystem (
        {
          pkgs,
          rust,
          wild-linker,
          inputs,
          ...
        }:
        {
          default = pkgs.mkShell.override { stdenv = wild-linker; } {
            nativeBuildInputs = inputs;
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath inputs;
          };
        }
      );

      packages = forEachSystem (
        { pkgs, inputs, ... }:
        {
          default =
            let
              manifest = pkgs.lib.importTOML ./Cargo.toml;
            in
            pkgs.rustPlatform.buildRustPackage {
              pname = manifest.package.name;
              version = manifest.package.version;
              src = pkgs.lib.cleanSource ./.;
              cargoLock.lockFile = ./Cargo.lock;

              nativeBuildInputs = inputs;
            };
        }
      );
    };
}
