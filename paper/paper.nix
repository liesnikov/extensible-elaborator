# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ stdenv, pandoc, texlive, librsvg, biber}:
let texlive-combined = texlive.combine { inherit (texlive)
      scheme-basic latexmk
      fontspec koma-script  # ??
      unicode-math # ??
      todonotes # for todo-pop-ups
      biblatex
      fancyvrb # fancy verbatim text
      acmart
      # acmart dependencies, for some reason not pulled from ctan
      babel
      booktabs
      caption
      cmap
      draftwatermark
      environ
      etoolbox
      # fontenc
      framed
      geometry
      # graphicx
      hyperref
      hyperxmp
      iftex
      ifmtarg
      libertine
      microtype
      natbib
      refcount
      setspace
      textcase
      totpages
      xkeyval
      xstring
      ncctools # manyfoot
      # newtxmath
      xcolor
      # zi4
      float
      comment
      fancyhdr
      pbalance
      preprint # balance
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
    make main.pdf
  '';

  installPhase = ''
    mkdir $out
    mv main.pdf $out/main.pdf
  '';

  TEXINPUTS =
    builtins.concatStringsSep ":" ([ "." ] ++ extraTexInputs ++ [ "" ]);
})
