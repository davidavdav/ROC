## The default plot routine for an object of type "roc"
plot.roc <- function(r, nr=1, chull=T, type=ifelse(nrow(r) > 15, "l", "b"), traditional=F,
                     xlim=c(0,1), ylim=c(0,1), xlab=NULL, ylab=NULL, 
                     lwd=2,  ...) {
  stopifnot("roc" %in% class(r))
  if (chull) r <- r[r$chull,]
  if (missing(type))
      type <- ifelse(nrow(r) > 15, "l", "b")
  if(traditional) {
      y <- 1-r$pmiss
      xlabel <- ifelse(is.null(xlab), "false alarm rate", xlab)
      ylabel <- ifelse(is.null(ylab), "hit rate", ylab)
  } else {
      y <- r$pmiss
      xlabel <- ifelse (is.null(xlab), "false alarm probability", xlab)
      ylabel <- ifelse (is.null(ylab), "miss probability", ylab)
  }
  if (nr==1) {
    par(pty="s")
    plot(r$pfa, y, xlim=xlim, ylim=ylim, type=type, xlab=xlabel, ylab=ylabel, lwd=lwd, ...)
  } else {
    lines(r$pfa, y, type=type, col=nr, ...)
  }
  summary(r)
}
