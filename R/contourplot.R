### Positioning Method for the top of a group of points.
top.pieces <- function(d, ...){
  .Deprecated(msg="Contour labeling is deprecated in directlabels; use isoband labeled isolines (or metR).")
  label.pieces(which.max,0)(d, ...)
}

### Positioning Method for the bottom of a group of points.
bottom.pieces <- function(d, ...){
  .Deprecated(msg="Contour labeling is deprecated in directlabels; use isoband labeled isolines (or metR).")
  label.pieces(which.min,1)(d, ...)
}
