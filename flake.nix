{
  description = "TUI for reviewing error logs from AWS Cloudwatch ";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      ghcVersion = "965";
      compiler = "ghc${ghcVersion}";

      # default systems compatible with pre-commit-hooks.nix
      # https://github.com/cachix/pre-commit-hooks.nix/pull/122
      defaultSystems = [
        "aarch64-linux"
        "aarch64-darwin"
        "i686-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    in
    flake-utils.lib.eachSystem defaultSystems
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
          hsPkgs = pkgs.haskell.packages.${compiler}.override {
            overrides = hfinal: hprev: {
              cloudwatcher = hfinal.callCabal2nix "cloudwatcher" ./. { };
            };
          };

          stack = pkgs.symlinkJoin {
            # Stack will be available as the usual `stack` in terminal
            name = "stack";
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --no-nix \
                  --system-ghc \
                  --no-install-ghc \
                "
            '';
          };

        in
        rec {
          devShell = pkgs.mkShell {
            buildInputs = [
              pkgs.cabal-install
              pkgs.haskell.compiler.${compiler}
              pkgs.nixpkgs-fmt
              pkgs.ormolu
              pkgs.zlib
              pkgs.zlib.dev
              stack
            ];
          };

          formatter = pkgs.nixpkgs-fmt;
          packages = flake-utils.lib.flattenTree {
            cloudwatcher = hsPkgs.cloudwatcher;
          };

          defaultPackage = packages.cloudwatcher;

          apps = {
            cloudwatcher = {
              type = "app";
              program = "${self.packages.${system}.cloudwatcher}/bin/cloudwatcher";
            };

            default = self.apps.${system}.cloudwatcher;
          };
        });
}
