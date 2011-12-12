`legend.det` <-
function(legend, where="ur", order=1:length(legend), col,
                       lty) {
  if (where == "ur") {
    x <- nd(50)
    y <- nd(50)
    xj <- 1
    yj <- 1
  } else {
    x <- nd(0.1)
    y <- nd(0.1)
    xj <- 0
    yj <- 0
  }
  n <- length(legend)
  if (missing(col)) 
    col=1:n
  if (missing(lty))
    lty=rep(1,n)
  legend(x, y, xjust=xj, yjust=yj, legend=legend[order], col=col[order], lwd=2, bg="white", lty=lty[order])
}

