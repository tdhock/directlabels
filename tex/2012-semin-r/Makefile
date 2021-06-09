2012-03-29-HOCKING-directlabels-semin-r.pdf: 2012-03-29-HOCKING-directlabels-semin-r.tex iris-images.png
	pdflatex 2012-03-29-HOCKING-directlabels-semin-r
	pdflatex 2012-03-29-HOCKING-directlabels-semin-r
2012-03-29-HOCKING-directlabels-semin-r.tex: 2012-03-29-HOCKING-directlabels-semin-r.Rnw pointLabel.R
	echo 'library(knitr);knit("2012-03-29-HOCKING-directlabels-semin-r.Rnw")'|R --vanilla
iris-images.png: iris-images.R
	R --vanilla < iris-images.R