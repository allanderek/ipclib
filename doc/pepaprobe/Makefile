PEPA_FILES  =
GRAPH_FILES = graphics/nfaprobe.ps graphics/dfaprobe.ps graphics/mindfaprobe.ps \
              graphics/probe.ps
EPSTOPDF    = epstopdf
DOT         = dot
DOTPSTYPE   = -Tps


PROBE       = --probe "(a,b)?, c:start, d:stop"
PROBECMD    = ../../dist/build/pepaprobe/pepaprobe ${PROBE} ../../tests/basic/good/test01.pepa --output-dot


all: pepaprobemanual.pdf

pepaprobemanual.pdf: pepaprobemanual.dvi ${GRAPH_FILES}
	dvipdf pepaprobemanual.dvi pepaprobemanual.pdf

pepaprobemanual.dvi: pepaprobemanual.tex ${GRAPH_FILES}
	latex pepaprobemanual
	bibtex pepaprobemanual
	latex pepaprobemanual


# graphics/nfaprobe.ps: graphics/nfaprobe.dot
# 	$(DOT) $(DOTPSTYPE) graphics/nfaprobe.dot -o graphics/nfaprobe.ps

graphics/%.ps: graphics/%.dot
	$(DOT) $(DOTPSTYPE) $< -o $@

graphics/nfaprobe.dot:
	${PROBECMD}  --no-self-loops --no-minimise --non-deterministic > $@

graphics/dfaprobe.dot:
	${PROBECMD}  --no-self-loops --no-minimise > $@

graphics/mindfaprobe.dot:
	${PROBECMD}  --no-self-loops > $@

graphics/probe.dot:
	${PROBECMD} > $@

clean:
	rm -f *~ *.dvi *.pdf *.log *.aux *.toc *.blg *.bbl
