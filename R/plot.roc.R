## The default plot routine for an object of type "roc"
plot.roc <- function(r, n=1, chull=T, type=ifelse(nrow(r) > 15, "l", "b"),
                     xlim=c(0,1), ylim=c(0,1), xlab="false alarm probability",
                     ylab="miss probability", ...) {
  stopifnot("roc" %in% class(r))
  if (chull) r <- r[r$chull,]
  if (n==1) {
    par(pty="s")
    plot(r$pfa, r$pmiss, xlim=xlim, ylim=ylim, type=type, xlab=xlab, ylab=ylab, ...)
  } else {
    lines(r$pfa, r$pmiss, type=type, col=n)
  }
  summary(r)
}
