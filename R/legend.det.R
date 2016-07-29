`legend.det` <-
function(legend, where="ur", order=1:length(legend), col,
                       lty, pch, min=0.1, max=50, ...) {
  if (where == "ur") {
    x <- nd(max)
    y <- nd(max)
    xj <- 1
    yj <- 1
  } else if (where == "ll") {
    x <- nd(min)
    y <- nd(min)
    xj <- 0
    yj <- 0
  } else {
      return(NULL)
  }
  n <- length(legend)
  if (missing(col)) 
    col=1:n
  if (missing(lty))
      lty=rep(1,n)
  legend(x, y, xjust=xj, yjust=yj, legend=legend[order], col=col[order], lwd=2, bg="white", lty=lty[order], ...)
}

