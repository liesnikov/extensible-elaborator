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
  src = ./.;

  phases = ["buildPhase"];
  buildPhase = ''
    pandoc --verbose --to=pdf --standalone \
           --bibliography=$src/bib.bib --biblatex --csl=$src/default.csl \
            -o $out $src/main.md
'';

  nativeBuildInputs =
    [ pandoc texlive-combined biber] ++  extraBuildInputs;

  FONTCONFIG_FILE = makeFontsConf { fontDirectories = [source-sans-pro source-code-pro source-serif-pro]; };

  TEXINPUTS =
    builtins.concatStringsSep ":" ([ "." ] ++ extraTexInputs ++ [ "" ]);
})
