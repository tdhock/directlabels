#!/bin/bash
python make_sweave.py HOCKING-latticedl-semin-r-in.Rnw > HOCKING-latticedl-semin-r.Rnw && \
 R CMD Sweave HOCKING-latticedl-semin-r.Rnw && \
 python killplot.py HOCKING-latticedl-semin-r.tex && \
 cp HOCKING-latticedl-semin-r.tex.new HOCKING-latticedl-semin-r.tex && \
 pdflatex HOCKING-latticedl-semin-r && \
 evince HOCKING-latticedl-semin-r.pdf &
