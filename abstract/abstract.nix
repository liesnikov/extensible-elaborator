# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ stdenv, pandoc, texlive,  librsvg, biber}:
let texlive-combined = texlive.combine { inherit (texlive)
      scheme-basic latex latexmk
      fontspec koma-script  # ??
      unicode-math # ??
      xcolor # for coloured text
      todonotes # for todo-pop-ups
      etoolbox
      biblatex
      hyperref # for references and links
      fancyvrb # fancy verbatim text
      # for easychair.cls
      footmisc
      listings
      mathtools
      lastpage
      eso-pic # for debug
    ; };
    extraTexInputs = [ ];
    extraBuildInputs = [ librsvg biber];
in stdenv.mkDerivation ({

  name = "extensible-elaborator-paper";
  src = builtins.filterSource
    (path: type: !(builtins.elem (builtins.baseNameOf path) ["main.pdf" "result"]))
    ./.;

  nativeBuildInputs =
    [ pandoc texlive-combined ] ++  extraBuildInputs;

  buildPhase = ''
    make main.tex
    make main.pdf
  '';

  installPhase = ''
    mkdir $out
    mv main.tex $out/main.tex
    mv main.pdf $out/main.pdf
  '';

  TEXINPUTS =
    builtins.concatStringsSep ":" ([ "." ] ++ extraTexInputs ++ [ "" ]);
})
