GRAPH_FILES = 
TEX_FILES   = options.tex concreteSyntax.tex
EPSTOPDF    = epstopdf

SMC          = ../../dist/build/smc/smc
SMCMANUAL    = ../../dist/build/smcmanual/smcmanual

all: srmcmanual.pdf srmctutorial.pdf

srmctutorial.pdf: srmctutorial.tex $(TEX_FILES) $(GRAPH_FILES)
	pdflatex srmctutorial

srmcmanual.pdf: srmcmanual.tex $(TEX_FILES) $(GRAPH_FILES)
	pdflatex srmcmanual
#	bibtex   srmcmanual
#	pdflatex srmcmanual

# options.tex is produced from the source code
options.tex: 
	$(SMCMANUAL) > ../../doc/srmc/options.tex

clean:
	rm -f *~ *.dvi *.pdf *.log *.aux *.toc *.blg *.bbl options.tex
