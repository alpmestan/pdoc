with (import <nixpkgs> { config.allowBroken = true; });

let tex = texlive.combined.scheme-medium;
    hs  = haskellPackages;
    py  = pkgs.python3.withPackages (python-pkgs: [
      python-pkgs.numpy
      python-pkgs.matplotlib
    ]);
    pdoc-list = hs.callCabal2nix "pdoc-list" ../pdoc-list {};
    pandocPkgs = [ pandoc
                   hs.pandoc-sidenote
                   pandoc-imagine
                   hs.pandoc-plot
                   pdoc-list
                   d2
                   librsvg
                   diagrams-pandoc
                 ];
in
mkShell {
  name = "pdoc";
  buildInputs = [ tex py watchexec ] ++ pandocPkgs;
}
