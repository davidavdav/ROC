plot.ddensity <- function(d, thres=NULL, xlab="score", main="PDFs", lwd=2, col=c("red", "blue"), legend=c("targets", "non-targets"), ...) {
  if ("sre" %in% class(d)) d <- ddensity(d)
  yr <- range(c(d[[1]]$y, d[[2]]$y))
  plot(d[["tar.d"]], ylim=yr, lwd=lwd, col=col[1], xlab=xlab, main=main, ...)
  lines(d[["non.d"]], lwd=lwd, col=col[2])
  for (i in 1:2) {
    modei <- which.max(d[[i]]$y)
    mode.x <- d[[i]]$x[modei]
    mode.y <- d[[i]]$y[modei]
    text(mode.x, mode.y, legend[i], pos=2*(3-i), col=col[i])
  }
}
       
