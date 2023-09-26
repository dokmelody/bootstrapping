
{
  description = "Development environment for pass-01, using `nix develop`.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      system = "x86_64-linux";
    in rec {
        devShells.${system}.default = pkgs.mkShell {
          buildInputs = [
            pkgs.sbcl

            pkgs.plan9port
            pkgs.rc
            pkgs.redo-apenwarr
          ];
       };
    };
}
