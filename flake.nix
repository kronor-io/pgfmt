{
  description = "Format PostgreSQL, checked against PostgreSQL's own parser";

  # What CI builds is pushed here (.github/workflows/check.yml).
  nixConfig = {
    extra-substituters = "https://kronor-open.cachix.org";
    extra-trusted-public-keys = "kronor-open.cachix.org-1:D1shHZh5BRkmM8RB9BaEqBURIgD/n5+u8KFXD1+DbF8=";
  };

  inputs = {
    haskell-nix.url = "github:kronor-io/haskell.nix/nixpkgs-ghc-601a4d19";
    nixpkgs.url = "github:NixOS/nixpkgs/release-26.05";
    haskell-nix.inputs.hackage.follows = "hackage-nix";
    haskell-nix.inputs.nixpkgs.follows = "nixpkgs";
    hackage-nix.url = "github:kronor-io/hackage.nix";
    hackage-nix.flake = false;
    kronor-haskell-packages.url = "github:kronor-io/kronor-haskell-packages/repo";
    kronor-haskell-packages.flake = false;
    # The commit lint/cabal.project names.
    hopinion.url = "github:NorfairKing/hopinion/48eb343a003ab05f7df0c5844ba37897ab35f2e7";
    hopinion.flake = false;
  };

  outputs = { self, haskell-nix, hackage-nix, kronor-haskell-packages, nixpkgs, hopinion }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      # libpg_query 18, matching the protobuf the libpg-query package was
      # generated from: a mismatched major decodes parse trees into the wrong
      # node types. Same override as kronor-io/pg-query's flake.
      libpgQueryOverlay = final: prev: {
        libpg_query = prev.libpg_query.overrideAttrs (_: {
          version = "18.0.0";
          src = prev.fetchFromGitHub {
            owner = "pganalyze";
            repo = "libpg_query";
            tag = "18.0.0";
            hash = "sha256-Fs9SFs8ramKYdkv1gEOMJd9SnLmKDcbf+zYKv1hHBfc=";
          };
          # The deparser writes a SET value that is not a plain identifier as
          # a double-quoted identifier (`set lock_timeout to "50ms"`); the
          # patch writes it as a string literal ('50ms') instead.
          patches = [ ./nix/libpg_query-set-values-as-strings.patch ];
          # pg_query.h includes postgres_deparse.h, which upstream's install
          # leaves out; and the shared library's SONAME needs a file by that
          # name (see kronor-io/pg-query's flake for the details).
          postInstall = ''
            install -Dm644 postgres_deparse.h -t "$out/include"
            names=$(make -s --eval='__pg_query_names: ; @echo $(SONAME) $(SOLIBVER) $(SOLIB)' __pg_query_names)
            set -- $names
            cd "$out/lib"
            [ "$1" = "$3" ] || ln -s "$3" "$1"
            [ "$2" = "$3" ] || [ "$2" = "$1" ] || ln -s "$3" "$2"
          '' + prev.lib.optionalString prev.stdenv.hostPlatform.isDarwin ''
            # Upstream links the dylib with a bare install name, so whatever
            # links it cannot find it at run time; point it at the store.
            install_name_tool -id "$out/lib/$1" "$out/lib/$3"
          '';
        });
        pg_query = final.libpg_query;
      };

      perSystem = system:
        let
          haskellNix = import haskell-nix {
            inherit system;
            sourcesOverride.hackage = hackage-nix;
          };

          pkgs = import nixpkgs {
            inherit system;
            overlays = [ haskellNix.overlay libpgQueryOverlay ];
          };

          # cabal.project builds against Hackage's postgresql-syntax;
          # cabal.project.kronor against Kronor's fork, with the kronor-syntax
          # flag for its hasql-th extensions.
          mkProject = cabalProjectFileName: pkgs.haskell-nix.cabalProject {
            src = ./.;
            inherit cabalProjectFileName;
            compiler-nix-name = "ghc9124";
            inputMap = {
              "https://kronor-io.github.io/kronor-haskell-packages" = kronor-haskell-packages;
            };
            modules = [{
              packages.libpg-query.components.library.libs = [ pkgs.libpg_query ];
              packages.pgstyle.components.library.libs = [ pkgs.libpg_query ];
            }];
          };

          project = mkProject "cabal.project";
          kronor = mkProject "cabal.project.kronor";

          # pgstyle's hopinion rules (lint/). Built with pgstyle's compiler,
          # since hopinion reads the .hie and .hi files that compiler writes.
          lintProject = pkgs.haskell-nix.cabalProject' {
            src = ./lint;
            compiler-nix-name = "ghc9124";
            # hopinion commits its .cabal files; without its package.yaml files
            # haskell.nix reads those rather than running hpack, which this
            # haskell.nix does not provide.
            inputMap = {
              "https://github.com/NorfairKing/hopinion/48eb343a003ab05f7df0c5844ba37897ab35f2e7" = pkgs.runCommand "hopinion-source" { } ''
                cp -r ${hopinion} $out
                chmod -R u+w $out
                find $out -name package.yaml -delete
              '';
            };
          };
          pgstyle-lint = lintProject.hsPkgs.pgstyle-lint.components.exes.pgstyle-lint;
        in
        {
          inherit pkgs project pgstyle-lint;
          pgstyle = project.pgstyle.components.exes.pgstyle;
          pgstyle-kronor = kronor.pgstyle.components.exes.pgstyle;
          checks = project.pgstyle.checks // {
            render-kronor = kronor.pgstyle.checks.render;
            file-kronor = kronor.pgstyle.checks.file;
            haskell-kronor = kronor.pgstyle.checks.haskell;
            layout-kronor = kronor.pgstyle.checks.layout;
          };
        };

      githubPlatforms = {
        "x86_64-linux" = "ubuntu-latest";
        "aarch64-linux" = "ubuntu-24.04-arm";
        "aarch64-darwin" = "macos-latest";
      };

      bySystem = forAllSystems perSystem;
    in
    {
      packages = forAllSystems (system: rec {
        pgstyle = bySystem.${system}.pgstyle;
        pgstyle-kronor = bySystem.${system}.pgstyle-kronor;
        pgstyle-lint = bySystem.${system}.pgstyle-lint;
        default = pgstyle;
      });

      checks = forAllSystems (system: bySystem.${system}.checks);

      githubActions.matrix.include = map
        (system: {
          inherit system;
          os = githubPlatforms.${system};
        })
        systems;

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${bySystem.${system}.pgstyle}/bin/pgstyle";
        };
      });

      devShells = forAllSystems (system:
        let inherit (bySystem.${system}) pkgs project pgstyle-lint; in {
          default = project.shellFor {
            tools.cabal = { version = "3.16.1.0"; };
            # scripts/check runs these.
            tools.fourmolu = { version = "0.19.0.1"; };
            tools.hlint = { version = "3.10"; };
            buildInputs = [ pkgs.libpg_query pgstyle-lint ];
          };
        });
    };
}
