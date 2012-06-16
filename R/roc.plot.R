roc.plot <- function(d, nr=1, lty=1, col=nr, xmin=0, xmax, ymin, ymax, traditional=F, rocch=F, ...) {
  if ("sre" %in% class(d)) d <- det(d)
  else stopifnot("det" %in% class(d))
  if (missing(xmax)) {
    min <- max(which(d$miss==0))
    xmax <- d$fa[min]
  }
  max <- min(which(d$fa==0))
  if (missing(ymax)) {
    if (traditional) ymax <- 1
    else ymax <- d$miss[max]
  }
  if (missing(ymin)) {
    if (traditional) ymin <- 1-d$miss[max]
    else ymin <- 0
  }
  if (traditional) {
    y <- 1-d$miss
    ylab <- "hit rate"
  } else {
    y <- d$miss
    ylab <- "miss rate"
  }
  if (rocch) {
    d$fa <- d$fa[d$ch]
    y <- y[d$ch]
  }
  if (nr==1)
    plot(d$fa, y, type="l", xlim=c(0,xmax), ylim=c(ymin,ymax), xlab="false alarm rate", ylab=ylab, main="ROC", lty=lty, col=col, ...)
  else
    lines(d$fa, y, lty=lty, col=col, ...)
}
