
{
  description = "Install all tools for generating Dokmelody documentation using 'nix develop'";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      system = "x86_64-linux";
      version = "4.1.1";

      plugin1 = pkgs.fetchurl {
        url = "https://github.com/doctales/org.doctales.reveal/archive/master.zip";
        sha256 = "sha256-MTsuRydhegeCTaUAvYjiqbbviH+/xiiEkXzzlJTzyyk=";
      };

      plugin2 = pkgs.fetchurl {
        url = "https://github.com/jason-fox/fox.jason.extend.css/archive/v1.2.1.zip";
        sha256 = "142cdam1rv4cracdm4fi18fwdmazy3qqraimkdk9lhkxq9av5cil";
      };

      plugin3 = pkgs.fetchurl {
        url = "https://github.com/infotexture/dita-bootstrap/archive/5.3.2.zip";
        sha256 = "1a5cwl6max7jny97r9syl6gss661ya8x317cvk3v1ss5linyyabx";
      };

      plugin4 = pkgs.fetchurl {
        url = "https://github.com/jason-fox/fox.jason.prismjs/archive/master.zip";
        sha256 = "0wslisnji8zb15b93bddvx5jar1s2xavp68wlrn1cb1mris9qd6m";
      };

      plugin5 = ./dependencies/com.oxygenxml.diagrams.svg.zip;

    in rec {
      packages.${system}.dita-for-dokmelody = pkgs.stdenv.mkDerivation rec {
          name = "dita-for-dokmelody";
          src = pkgs.fetchurl {
           url = "https://github.com/dita-ot/dita-ot/releases/download/${version}/dita-ot-${version}.zip";
           sha256 = "sha256-qGrpSZ3bp/lwPhrj7qpuqfg7ihKOxsSwGtr4A8+dHPs=";
          };

          buildInputs = [
            pkgs.jdk
            pkgs.unzip
            pkgs.coreutils
            pkgs.plantuml
          ];

          dontConfigure = true;
          dontBuild = true;
          installPhase = ''
            runHook preInstall
            mkdir -p $out
            cp -a * $out/
            cd $out
            ./bin/dita install ${plugin1}
            ./bin/dita install ${plugin2}
            ./bin/dita install ${plugin3}
            ./bin/dita install ${plugin4}
            ./bin/dita install ${plugin5}
            runHook postInstall
          '';
        };

        devShells.${system}.default = pkgs.mkShell {
          buildInputs = [
            packages.${system}.dita-for-dokmelody
            pkgs.plantuml
            pkgs.jdk
            pkgs.unzip
            pkgs.jdk

            pkgs.hugo
            pkgs.pandoc
            pkgs.nodejs
            pkgs.nodePackages.prettier

            pkgs.plan9port
            pkgs.rc
            pkgs.rsync
            pkgs.python3

            pkgs.redo-apenwarr
          ];
       };
    };
}
