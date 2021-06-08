# http://en.wikipedia.org/wiki/Automatic_label_placement
# http://www.szoraster.com/Cartography/PracticalExperience.htm
# http://www.eecs.harvard.edu/~shieber/Projects/Carto/carto.html
# http://i11www.iti.uni-karlsruhe.de/map-labeling/bibliography/


pointLabel <- function(x, y = NULL, labels = seq(along = x), cex = 1,
                       method = c("SANN", "GA"),
                       allowSmallOverlap = FALSE,
                       trace = FALSE,
                       doPlot = TRUE,
                       ...)
{
  if (!missing(y) && (is.character(y) || is.expression(y))) {
    labels <- y
    y <- NULL
  }
  if (is.factor(labels)) 
    labels <- as.character(labels)
  z = xy.coords(x, y, recycle = TRUE)
  x = z$x
  y = z$y
  if (length(labels) < length(x))
    labels = rep(labels, length(x))
  
  method <- match.arg(method)
  
  boundary = par()$usr
  image_width = boundary[2] - boundary[1]
  image_height = boundary[4] - boundary[3]
  if (allowSmallOverlap) # default to 2% of the image size
    nudgeFactor = .02*(abs(boundary[1] + 1i*boundary[2] - boundary[3] - 1i*boundary[4]))

  n_labels = length(x)
                         
  # There are eight possible alignment codes, corresponding to the 
  # corners and side mid-points of the rectangle
  # Codes are 1:8
  # Code 7 is the most preferred
  xBoundary = image_width * 0.01 # add a small boundary around the rectangle
  yBoundary = image_height * 0.01
  width = strwidth(labels, units = "user", cex = cex) + xBoundary
  height = strheight(labels, units = "user", cex = cex) + yBoundary
  gen_offset <- function(code)
         c(-1, -1, -1,  0,  0,  1,  1,  1)[code] * (width/2) +
    1i * c(-1,  0,  1, -1,  1, -1,  0,  1)[code] * (height/2)
  
  
  # Finds intersection area of two rectangles
  rect_intersect <- function(xy1, offset1, xy2, offset2) {
    w = pmin(Re(xy1+offset1/2), Re(xy2+offset2/2)) - pmax(Re(xy1-offset1/2), Re(xy2-offset2/2))   
    h = pmin(Im(xy1+offset1/2), Im(xy2+offset2/2)) - pmax(Im(xy1-offset1/2), Im(xy2-offset2/2))   
    w[w <= 0] = 0
    h[h <= 0] = 0
    w*h
  }
  
  nudge <- function(offset) {
    # Nudge the labels slightly if they overlap:
    doesIntersect = rect_intersect(xy[rectidx1] + offset[rectidx1], rectv[rectidx1],
                                   xy[rectidx2] + offset[rectidx2], rectv[rectidx2]) > 0
  
    pyth = abs(xy[rectidx1] + offset[rectidx1] - xy[rectidx2] - offset[rectidx2]) / nudgeFactor
    eps = 1.0e-10

    for (i in which(doesIntersect & pyth > eps)) {
      idx1 = rectidx1[i]
      idx2 = rectidx2[i]
      vect = (xy[idx1] + offset[idx1] - xy[idx2] - offset[idx2]) / pyth[idx1]
      offset[idx1] = offset[idx1] + vect
      offset[idx2] = offset[idx2] - vect
    }
    offset
  }
  
  objective <- function(gene) {
    offset = gen_offset(gene)

    # Allow for "bending" the labels a bit
    if (allowSmallOverlap) offset = nudge(offset)

    if (!is.null(rectidx1))
      area = sum(rect_intersect(xy[rectidx1] + offset[rectidx1], rectv[rectidx1],
                                xy[rectidx2] + offset[rectidx2], rectv[rectidx2]))
    else
      area = 0
      
    # Penalize labels which go outside the image area
    # Count points outside of the image
    n_outside = sum(Re(xy + offset - rectv/2) < boundary[1] | Re(xy + offset + rectv/2) > boundary[2] |
                    Im(xy + offset - rectv/2) < boundary[3] | Im(xy + offset + rectv/2) > boundary[4]) 
    area + n_outside * image_width * image_height
  }
  
   
  # Make a list of label rectangles in their reference positions,
  # centered over the map feature; the real labels are displaced
  # from these positions so as not to overlap
  # Note that some labels can be bigger than others
  xy = x + 1i * y
  rectv = width + 1i * height

  rectidx1 = rectidx2 = array(0, (length(x)^2 - length(x)) / 2)
  k=0
  for (i in 1:length(x))
    for (j in seq(len=(i-1))) {
      k = k + 1
      rectidx1[k] = i
      rectidx2[k] = j
    }
  canIntersect = rect_intersect(xy[rectidx1], 2 * rectv[rectidx1],
                                xy[rectidx2], 2 * rectv[rectidx2]) > 0
  rectidx1 = rectidx1[canIntersect]
  rectidx2 = rectidx2[canIntersect]
  if (trace) cat("possible intersects =", length(rectidx1), "\n")

  if (trace) cat("portion covered =", sum(rect_intersect(xy, rectv,xy,rectv))/(image_width*image_height),"\n")

  GA <- function() {
    # Make some starting genes
    n_startgenes = 1000     # size of starting gene pool 
    n_bestgenes = 30       # genes selected for cross-breeding
    prob = 0.2

    # Mutation function: O(n^2) time
    mutate <- function(gene) {
      offset = gen_offset(gene)
      # Directed mutation where two rectangles intersect
      doesIntersect = rect_intersect(xy[rectidx1] + offset[rectidx1], rectv[rectidx1],
                                     xy[rectidx2] + offset[rectidx2], rectv[rectidx2]) > 0
    
      for (i in which(doesIntersect)) {
        gene[rectidx1[i]] = sample(1:8, 1)
      }
      # And a bit of random mutation, too
      for (i in seq(along=gene))
        if (runif(1) <= prob)
          gene[i] = sample(1:8, 1)
      gene
    }
    
    # Crossbreed two genes, then mutate at "hot spots" where intersections remain
    crossbreed <- function(g1, g2)
      ifelse(sample(c(0,1), length(g1), repl = TRUE) > .5, g1, g2)


    genes = matrix(sample(1:8, n_labels * n_startgenes, repl = TRUE), n_startgenes, n_labels)
    
    for (i in 1:10) {
      scores = array(0., NROW(genes))
      for (j in 1:NROW(genes))
        scores[j] = objective(genes[j,])
      rankings = order(scores)
      genes = genes[rankings,]
      bestgenes = genes[1:n_bestgenes,]
      bestscore = scores[rankings][1]
      if (bestscore == 0) {
        if (trace) cat("overlap area =", bestscore, "\n")
        break
      }
      # At each stage, we breed the best genes with one another
      genes = matrix(0, n_bestgenes^2, n_labels)
      for (j in 1:n_bestgenes)
        for (k in 1:n_bestgenes)
          genes[n_bestgenes*(j-1) + k,] = mutate(crossbreed(bestgenes[j,], bestgenes[k,]))
      
      genes = rbind(bestgenes, genes)
      if (trace) cat("overlap area =", bestscore, "\n")
    }
    nx = Re(xy + gen_offset(bestgenes[1,]))
    ny = Im(xy + gen_offset(bestgenes[1,]))
    list(x = nx, y = ny)
  }
  SANN <- function() {
    # Make some starting "genes"
    #gene = sample(1:8, n_labels, repl = TRUE)
    gene = rep(8, n_labels)
    score = objective(gene)
    bestgene = gene
    bestscore = score
    T = 2.5
    for (i in 1:50) {
      k = 1
      for (j in 1:50) {
        newgene = gene
        newgene[sample(1:n_labels, 1)] = sample(1:8,1)
        newscore = objective(newgene)
        if (newscore < score || runif(1) < 1 - exp((newscore - score) / T)) {
          k = k + 1
          score = newscore
          gene = newgene
        }
        if (score <= bestscore) {
          bestscore = score
          bestgene = gene
        }
        if (bestscore == 0 || k == 10) break
      }
      if (bestscore == 0) break
      if (trace) cat("overlap area =", bestscore, "\n")
      T = 0.9 * T
    }
    
    if (trace) cat("overlap area =", bestscore, "\n")
    nx = Re(xy + gen_offset(bestgene))
    ny = Im(xy + gen_offset(bestgene))
    list(x = nx, y = ny)
  }
  if (method == "SANN")
    xy = SANN()
  else
    xy = GA()
  if (doPlot)
    text(xy, labels, cex = cex, ...)
  invisible(xy)
}

