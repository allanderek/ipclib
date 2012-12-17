PEPA_FILES   = tiny.pepa
GRAPH_FILES  = simpleTea-cdf.pdf simpleTea-pdf.pdf
EPSTOPDF     = epstopdf
LHS2TEX      = lhs2TeX

IPC          = ../../../dist/build/ipc/ipc
IPCMANUAL    = ../../../dist/build/ipcmanual/ipcmanual

simpleTea-cdf.pdf simpleTea-pdf.pdf: simpleTea.pepa
	$(IPC) simpleTea.pepa --source thirst --target stir --graph-output pdf --line-width=3.0