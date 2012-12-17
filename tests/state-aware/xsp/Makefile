PEPA_FILES  =
GRAPH_FILES = queue_cdf.pdf queue_pdf.pdf busy_cdf.pdf busy_pdf.pdf \
              random_pdf.pdf random_cdf.pdf compare_pdf.pdf compare_cdf.pdf \
              token_states.pdf
EPSTOPDF    = eps2pdf -f 

# To use hydra
#IPC           = ../../../dist/build/ipc/ipc --run-hydra
# or
#IPC            = ../../../dist/build/ipc/ipc --hydra-stage flat-mod
#OUTPUTFLAG    = --mod-file
#OUTPUTSUFFIX  = .mod
#RESULTSSUFFIX = .PT_RESULTS

IPC           = ../../../dist/build/ipc/ipc
OUTPUTFLAG    = --output
OUTPUTSUFFIX  = _cdf.csv
RESULTSSUFFIX = _cdf.csv

MAIN_TARGET   = xsp.pdf



all: $(MAIN_TARGET)

xsp.pdf: xsp.tex ${GRAPH_FILES}
	pdflatex xsp
	bibtex xsp
	pdflatex xsp


### Very small example
small_pdf.pdf small_cdf.pdf: small_xsp.pepa small.exper
	$(IPC) small.exper

### Queueing example
queue_pdf.pdf queue_cdf.pdf: queue.pepa queue.exper
	$(IPC) queue.exper

### Token Ring example
busy_pdf.pdf busy_cdf.pdf: busy.pepa busy.exper
	$(IPC) busy.exper

### State space of the token example
token_states.pdf: token_states.dot
	dot -Tps -o token_states.eps token_states.dot
	$(EPSTOPDF) token_states.eps

### Random queue example
random_pdf.pdf random_cdf.pdf: random.pepa random.exper
	$(IPC) random.exper

### Comparison
compare_pdf.pdf compare_cdf.pdf: random.pepa queue.pepa busy.pepa compare.exper
	$(IPC) compare.exper

clean:
	rm -f *~ *.dvi *.log *.aux *.toc *.blg *.bbl *$(RESULTSSUFFIX) *.csv *.eps
	rm -f $(GRAPH_FILES) $(MAIN_TARGET)
	remove_hydra_intermediates
