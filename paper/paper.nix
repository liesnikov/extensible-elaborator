# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ stdenv, lib, pandoc, texlive, fontconfig, biber,
  makeFontsConf, source-serif-pro, source-sans-pro, source-code-pro}:
let texlive-combined = texlive.combine { inherit (texlive) scheme-basic xetex lm fontspec euenc koma-script unicode-math xcolor todonotes etoolbox biblatex hyperref; };
    extraTexInputs = [ ];
    extraBuildInputs = [ ];
in stdenv.mkDerivation ({

  name = "extensible-elaborator-paper";
  src = builtins.filterSource
    (path: type: !(builtins.elem (builtins.baseNameOf path) ["main.pdf" "result" "fonts.patch"]))
    ./.;

  patches = [./fonts.patch];

  buildPhase = ''
    pandoc --standalone main.md \
           --bibliography=bib.bib --biblatex --csl=default.csl \
           --pdf-engine=xelatex -o out.tex
    xelatex out
    biber out
    xelatex out
  '';


  installPhase = ''
    mv out.pdf $out
  '';
  nativeBuildInputs =
    [ pandoc texlive-combined biber] ++  extraBuildInputs;

  FONTCONFIG_FILE = makeFontsConf { fontDirectories = [source-sans-pro source-code-pro source-serif-pro]; };

  TEXINPUTS =
    builtins.concatStringsSep ":" ([ "." ] ++ extraTexInputs ++ [ "" ]);
})
