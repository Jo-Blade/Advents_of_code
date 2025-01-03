{
  description = "A program to export churros events to an online calendar.";

  # Nixpkgs / NixOS version to use.
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      # Generate a user-friendly version number.
      version = "alpha1";

      # System types to support.
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });

    in
    {

      # enable nix fmt
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;

      # Provide some binary packages for selected system types.
      packages = forAllSystems (system:
        let
          advent-of-code = pkgs: pkgs.ocamlPackages.buildDunePackage {
            inherit version;
            pname = "aoc";
            # Tell nix that the source of the content is in the root
            src = ./.;

            nativeBuildInputs = [ pkgs.ocamlPackages.menhir ];
            buildInputs = with pkgs.ocamlPackages; [
	      angstrom
            ];
          };

          dockerbuilder = buildfun: pkgs:
            let
              advent-of-code-package = advent-of-code pkgs;
            in
            buildfun {
              name = "advent-of-code";
              tag = "latest";
              contents = [ pkgs.cacert advent-of-code-package ];
              config.Cmd = [ "${advent-of-code-package}/bin/aoc" ];
              # IMPORTANT: MAKE HTTPS WORK
              config.Env =
                [
                  "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                ];
              # IMPORTANT: ocaml cohttp needs /etc/services, see https://github.com/mirage/ocaml-cohttp/issues/675
              enableFakechroot = true;
              fakeRootCommands = ''
                mkdir -p /etc
                cat <<EOF > /etc/services
                https            443/tcp    # http protocol over TLS/SSL
                https            443/udp    # http protocol over TLS/SSL
                https            443/sctp   # HTTPS
                EOF
              '';
            };
        in
        let
          pkgs = nixpkgsFor.${system};
        in
        {
          # Build the application bin using glibc
          advent-of-code = advent-of-code pkgs;
          advent-of-code-musl = advent-of-code pkgs.pkgsMusl;

          # Build docker compressed image using glibc
          docker-image = dockerbuilder pkgs.dockerTools.buildLayeredImage pkgs;
          # Build script to generate uncompressed docker image (use less space in nix store)
          docker-stream = dockerbuilder pkgs.dockerTools.streamLayeredImage pkgs;

          # Build docker compressed image using musl (resulting in smaller image
          # but much longer build as not all packages are available in nixos official cache)
          docker-image-musl = dockerbuilder pkgs.dockerTools.buildLayeredImage pkgs.pkgsMusl;
          # Build script to generate uncompressed docker image using musl
          docker-stream-musl = dockerbuilder pkgs.dockerTools.streamLayeredImage pkgs.pkgsMusl;
        });

      # Add dependencies that are only needed for development
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in
        {
          default = pkgs.mkShell {
            buildInputs = (with pkgs; [ nixd ocaml dune_3 ocamlformat ocamlPackages.ocaml-lsp ocamlPackages.utop ])
              ++ (with self.packages.${system}.advent-of-code; nativeBuildInputs ++ buildInputs);
          };
        });

      # The default package for 'nix build'. This makes sense if the
      # flake provides only one package or there is a clear "main"
      # package.
      defaultPackage = forAllSystems (system: self.packages.${system}.advent-of-code);
    };
}
