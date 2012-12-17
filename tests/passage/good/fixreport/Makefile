PEPA_FILES  =
GRAPH_FILES =
EPSTOPDF    = epstopdf
GRAPHEXT    = .pdf

all: passage-time-fix.pdf ../cdfk${GRAPHEXT} ../pdfk${GRAPHEXT} ../ratecdf${GRAPHEXT} ../ratepdf${GRAPHEXT}

passage-time-fix.pdf: passage-time-fix.tex
	pdflatex passage-time-fix
	bibtex passage-time-fix
	pdflatex passage-time-fix


clean:
	rm -f *~ *.dvi *.pdf *.log *.aux *.toc *.blg *.bbl
