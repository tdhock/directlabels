directlabels.pdf: directlabels.tex refs.bib
	pdflatex directlabels.tex
	bibtex directlabels
	pdflatex directlabels.tex
	pdflatex directlabels.tex
directlabels.tex: directlabels.Rnw 
	PATH=~/bin:$(PATH) R CMD Sweave directlabels.Rnw
