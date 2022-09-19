.PHONY: all pdf tex reset allclean clean open

all: pdf
pdf: main.pdf
tex: main.tex

reset: allclean pdf

main.pdf: main.tex Makefile
	latexmk -xelatex="xelatex -interaction=nonstopmode" -pdf -use-make main.tex

main.tex: main.md macros.tex Makefile
	pandoc --standalone main.md \
         --include-in-header=macros.tex \
         -t beamer \
         --bibliography bib.bib --biblatex \
         --pdf-engine=xelatex\
         --slide-level=2 \
         -o main.tex

open: main.pdf
	xdg-open main.pdf &

allclean: clean
	rm -f main.pdf main.tex

clean:
	rm -f *.aux *.log *.nav *.out *.snm *.toc *.vrb *.pk *.bbl *.blg *.bcf *.dvi *.fdb_latexmk *.fls *.run.xml

