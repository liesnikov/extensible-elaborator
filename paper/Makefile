all: nix-build

.PHONY: open reset

pdf: main.pdf
tex: main.tex
reset: clean pdf

open: main.pdf
	xdg-open main.pdf &

nix-build: main.tex Makefile bib.bib
	nix-build . ""

main.pdf: main.tex Makefile
	latexmk -pdf main.tex

#main.tex: main.md bib.bib Makefile
#	pandoc --standalone main.md \
#  --filter pandoc-secnos \
#  --bibliography bib.bib \
#  --natbib \
#  -o main.tex

allclean: clean
	rm -f main.pdf main.tex

clean:
	rm -f *.aux *.log *.nav *.out *.snm *.toc *.vrb *.pk *.bbl *.blg *.bcf *.dvi *.fdb_latexmk *.fls *.run.xml
