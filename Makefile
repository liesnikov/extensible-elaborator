all: nix-build

.PHONY: open reset

pdf: main.pdf
tex: main.tex
reset: clean pdf

open: main.pdf
	xdg-open main.pdf &

nix-build: main.md Makefile bib.bib
	nix-build . ""

main.pdf: main.tex body.tex Makefile
	latexmk -pdf main.tex

body.tex: main.md bib.bib Makefile
	pandoc main.md \
  --filter pandoc-secnos \
  --bibliography bib.bib \
  --natbib \
  -o body.tex

allclean: clean
	rm -f main.pdf main.tex

clean:
	rm -f *.aux *.log *.nav *.out *.snm *.toc *.vrb *.pk *.bbl *.blg *.bcf *.dvi *.fdb_latexmk *.fls *.run.xml
