.PHONY: tex clean

tex: main.tex

main.tex: main.md Makefile
	pandoc main.md \
	--filter pandoc-secnos \
	--natbib \
	--listings \
	-o main.tex

clean:
	rm -f main.tex *.aux *.log *.out *.toc *.fdb_latexmk *.fls

