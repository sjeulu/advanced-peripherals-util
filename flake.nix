{
  description = ''
    A mod that adds many useful extensions to ComputerCraft
    and aims to add features that were available in 1.12.2
    mods like PeripheralsPlusOne.
  '';

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.haskell-flake.flakeModule ];
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        {
          packages = {
            default = self'.packages.advanced-peripherals-util;
          };
          haskellProjects.default = with pkgs; {
            basePackages = haskell.packages.ghc96;
            projectRoot = ./.;
            autoWire = [ "packages" "apps" "devShells" ];
            settings.extraBuildDepends = [ zlib zip ];
            devShell.tools = hp: {
              ghc = haskell.compiler.ghc96;
              hlint = null;
              haskell-language-server = haskell-language-server.override {
                supportedGhcVersions = [ "96" ];
              };
            };
          };
        };
    };
}
