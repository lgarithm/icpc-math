
default: debug clear

debug:
	pdflatex icpcmath.tex

# build twice for correct generation of content
release:
	pdflatex icpcmath.tex
	makeindex icpcmath.idx
	pdflatex icpcmath.tex
	pdflatex icpcmath.tex
	makeindex icpcmath.idx
	pdflatex icpcmath.tex

clear:
	-rm *.aux
	-rm *.log
	-rm *.toc
