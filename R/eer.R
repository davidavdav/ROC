## Function to compute the EER through the convex hull

eer <- function(pfa, pmiss, index=NULL) {
  if (is.null(index)) {
    ## first, add a "corner" (1.1,1.1)
    n <- length(pfa)                      # number of points
    pfa <- c(pfa, 1.1)
    miss <- c(pmiss, 1.1)
    index <- chull(pfa, pmiss)               # find convex hull data points
    index <- sort(index[index<=n])          # remove corner
  }
  i <- which(diff(sign(pfa[index]-pmiss[index]))!=0)
  if (i==length(index))                   # this should not happen
    return ((pfa[i]+pmiss[i])/2)
  ## intersect convex hull segment with y=x
  ax <- pfa[index][i]
  bx <- pfa[index][i+1]
  ay <- pmiss[index][i]
  by <- pmiss[index][i+1]
  ax + (ax-ay)*(bx-ax) / (ax-ay-bx+by)
}
