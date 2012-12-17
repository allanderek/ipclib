PEPA_FILES   = models/tiny.pepa
GRAPH_FILES  = models/tiny_cdf.pdf models/tiny_pdf.pdf \
               models/simpleTea-cdf.pdf models/simpleTea-pdf.pdf
TEX_FILES    = main.tex macros.tex options.tex # lhs2TeX.sty
EPSTOPDF     = epstopdf
LHS2TEX      = lhs2TeX

IPC          = ../../dist/build/ipc/ipc
IPCMANUAL    = ../../dist/build/ipcmanual/ipcmanual

all: 
	cd models ; make
	make allpdfs

allpdfs: ipcmanual.pdf ipcdevmanual.pdf ipctutorial.pdf

ipcmanual.pdf: ipcmanual.tex $(TEX_FILES) $(GRAPH_FILES) simpleTea-steady-output
	pdflatex ipcmanual
	bibtex   ipcmanual
	pdflatex ipcmanual

ipcdevmanual.pdf: ipcdevmanual.tex $(TEX_FILES) $(GRAPH_FILES)
	pdflatex ipcdevmanual
	bibtex   ipcdevmanual
	pdflatex ipcdevmanual

ipctutorial.pdf: ipctutorial.tex $(TEX_FILES) $(GRAPH_FILES)
	pdflatex ipctutorial
	bibtex   ipctutorial
	pdflatex ipctutorial

simpleTea-steady-output: models/simpleTea.pepa
	$(IPC) models/simpleTea.pepa > $@

models/tiny_cdf.pdf models/tiny_pdf.pdf: models/tiny.pepa
	$(IPC) models/tiny.exper

# options.tex is produced from the source code
options.tex: $(IPCMANUAL)
	$(IPCMANUAL) > ../../doc/ipc/options.tex

lhs2TeX.sty: lhs2TeX.lhs
	$(LHS2TEX) -o $@ $^


clean:
	rm -f *~ *.dvi *.pdf *.log *.aux *.toc *.blg *.bbl models/*.eps models/*.pdf \
        models/*.gnuplot models/*.csv