directlabels.pdf: directlabels.tex figures.pdf
	pdflatex directlabels && xpdf directlabels.pdf
figures.pdf: directlabels.R
	R --no-save --slave < directlabels.R
