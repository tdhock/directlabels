### Useful for labeling lines that all end at the top.
angled.endpoints <- list("last.points",rot=30)

### Label points at the top, making sure they don't collide.
top.qp <- list(
  "top.points",
  "calc.boxes",
  qp.labels(
    "x","left","right",
    make.tiebreaker("y","x"),
    xlimits))

