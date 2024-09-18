with (import <nixpkgs> { config.allowBroken = true; });

let tex = texlive.combined.scheme-medium;
    hs  = haskellPackages;
    py  = pkgs.python3.withPackages (python-pkgs: [
      python-pkgs.numpy
      python-pkgs.matplotlib
      python-pkgs.pandas
      python-pkgs.bokeh
    ]);
    pdoc-list = hs.callCabal2nix "pdoc-list" ./pdoc-list {};
    pdoc = hs.callCabal2nix "pdoc" ./pdoc {};
    pdoc-watch = writeScriptBin "pdoc-watch" ''
      ${watchexec}/bin/watchexec --timings -c -w $1 -e md -- ${pdoc}/bin/pdoc -i$1 -o./_out
    '';
    ghcEnv = hs.ghcWithPackages (ps: with ps; [ diagrams diagrams-contrib diagrams-pandoc pandoc palette SVGFonts cassava vector bytestring plots ]);
    d-p = stdenv.mkDerivation rec {
        pname = "diagrams-pandoc-loaded";
        version = hs.diagrams-pandoc.version;
        name = pname + "-" + version;
        buildInputs = [ makeWrapper ];
        phases = [ "installPhase" ];
        installPhase = ''
          mkdir -p $out/bin
          makeWrapper \
            "${ghcEnv}/bin/diagrams-pandoc" \
            "$out/bin/diagrams-pandoc" \
              --set NIX_GHC ${ghcEnv}/bin/ghc \
              --set NIX_GHC_LIBDIR ${ghcEnv}/lib/ghc-${ghcEnv.version}
        '';
    };

    # pandoc, pdoc, tools & filters
    pandocPkgs = [ pandoc
                   hs.pandoc-sidenote
                   pandoc-imagine
                   hs.pandoc-plot
                   d-p
                   watchexec
                   pdoc
                   pdoc-list
                   pdoc-watch
                   d2
                   librsvg
                   ghcEnv
                 ];
in
mkShell {
  name = "pdoc";
  buildInputs = [ tex py ] ++ pandocPkgs;
}