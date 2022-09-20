.PHONY: all pdf tex reset allclean clean open figures

all: pdf
pdf: main.pdf
tex: main.tex

reset: allclean pdf

main.pdf: main.tex figures Makefile
	latexmk -xelatex="xelatex -interaction=nonstopmode" -pdfxe -use-make main.tex

main.tex: main.md macros.tex Makefile
	pandoc --standalone main.md \
         --pdf-engine=xelatex\
         --include-in-header=macros.tex \
         -t beamer \
         --bibliography bib.bib --biblatex \
         --slide-level=2 \
         -o main.tex

figures: dependent-types-compiler.pdf dependent-types-typechecker.pdf dependent-types-general.pdf prior-work.pdf

dependent-types-compiler.pdf:
	inkscape ./dependent-types-compiler.svg --export-pdf=dependent-types-compiler.pdf

dependent-types-typechecker.pdf:
	inkscape ./dependent-types-typechecker.svg --export-pdf=dependent-types-typechecker.pdf

dependent-types-general.pdf:
	inkscape ./dependent-types-general.svg --export-pdf=dependent-types-general.pdf

prior-work.pdf:
	inkscape ./prior-work.svg --export-pdf=prior-work.pdf

open: main.pdf
	xdg-open main.pdf &

allclean: clean
	rm -f main.pdf main.tex

clean:
	rm -f *.aux *.log *.nav *.out *.snm *.toc *.vrb *.pk *.bbl *.blg *.bcf *.dvi *.fdb_latexmk *.fls *.run.xml
