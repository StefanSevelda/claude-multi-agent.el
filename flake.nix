# Packages the plugin for Nix consumers — primarily local-setup, which pins
# this flake as an input and links the built module into
# $DOOMDIR/modules/tools/claude-multi (see local-setup's
# nix/modules/doom-emacs.nix).
#
# The output is deliberately NOT an ELPA-style emacsPackages build: Doom loads
# this as a *module* (`:tools claude-multi` in $DOOMDIR/init.el), so what a
# consumer needs is the module directory layout — config.el, init.el,
# packages.el, autoload/ — exactly what `make install` symlinks. Byte
# compilation stays Doom's job (`doom sync`).
{
  description = "Doom Emacs module: sessions table over Claude Code agents managed by the cma CLI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      # The machines this targets are aarch64-darwin (see local-setup); the
      # Linux systems keep `nix build`/`nix flake check` usable from CI and
      # Linux containers.
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
        "aarch64-linux"
      ];
      eachSystem = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      version = if (self ? shortRev) then "0-unstable-${self.shortRev}" else "0-unstable-dirty";
    in
    {
      packages = eachSystem (pkgs: rec {
        default = claude-multi-agent;

        # Only the four things Doom reads are installed; docs, tests and the
        # vendored .packages/ tree stay out of the closure.
        claude-multi-agent = pkgs.stdenvNoCC.mkDerivation {
          pname = "claude-multi-agent";
          inherit version;
          src = self;

          dontConfigure = true;
          dontBuild = true;

          installPhase = ''
            runHook preInstall
            mkdir -p $out/autoload
            cp config.el init.el packages.el $out/
            cp autoload/*.el $out/autoload/
            runHook postInstall
          '';

          meta = {
            description = "Doom Emacs sessions table over parallel Claude Code agents (cma CLI)";
            homepage = "https://github.com/StefanSevelda/claude-multi-agent.el";
            license = nixpkgs.lib.licenses.mit;
          };
        };
      });

      checks = eachSystem (pkgs: {
        # Same guard the repo's pre-commit tooling gives locally: every .el
        # file must at least read as balanced Emacs Lisp. Doom macros (map!,
        # use-package!) make full byte-compilation outside Doom meaningless,
        # so this stops at syntax on purpose.
        parens =
          pkgs.runCommand "claude-multi-agent-parens" { nativeBuildInputs = [ pkgs.emacs-nox ]; }
            ''
              emacs -Q --batch \
                --eval '(progn
                          (dolist (f command-line-args-left)
                            (with-temp-buffer
                              (insert-file-contents f)
                              (emacs-lisp-mode)
                              (check-parens))
                            (message "OK %s" f))
                          (setq command-line-args-left nil))' \
                ${self}/config.el ${self}/init.el ${self}/packages.el ${self}/autoload/*.el
              touch $out
            '';

        # `make test`, hermetically: the Makefile clones buttercup/dash/s/f
        # from GitHub into .test-deps; here the same libraries come from
        # nixpkgs and land on the load-path via the emacsWithPackages
        # site-start (which is why this uses -batch, not -Q).
        tests =
          pkgs.runCommand "claude-multi-agent-tests"
            {
              nativeBuildInputs = [
                (pkgs.emacs-nox.pkgs.withPackages (epkgs: [
                  epkgs.buttercup
                  epkgs.dash
                  epkgs.s
                  epkgs.f
                ]))
              ];
            }
            ''
              cd ${self}
              emacs -batch \
                -L . \
                -L autoload \
                -L test \
                -l buttercup \
                -l test/test-simple.el \
                -l test/test-cma-commands.el \
                -l test/test-cma-table.el \
                -f buttercup-run
              touch $out
            '';
      });

      formatter = eachSystem (pkgs: pkgs.nixfmt-rfc-style);
    };
}
