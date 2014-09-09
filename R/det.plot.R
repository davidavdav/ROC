## Same as plot.det, but new implementation, hopefully with less potential problems.
## This function takes objecs of class data.frame, cst, and roc. 

det.plot <- function(x, nr=1, lty=1, col=nr, 
         xmin=0.1, xmax=50, ymin=0.1, ymax=50,
         xlab="false alarm probability (%)",
         ylab="miss probability (%)",
         chull=F, Ninterp=10,
         ...) {
  if (is.null(x)) {
      xdata <- ydata <- numeric(0)
      ptype <- "b"
  }
  else {
      x <- as.roc(x)
      ## x should be of class "roc" now
      xdata <- qnorm(x$pfa)
      ydata <- qnorm(x$pmiss)
      ptype <- ifelse(nrow(x) > 15, "l", "b")
  }
  xlim <- c(nd(xmin), nd(xmax))         # 0.5 % seems accurately enough
  ylim <- c(nd(ymin), nd(ymax))
  xdata <- limit.quantile(xdata, xlim)
  ydata <- limit.quantile(ydata, ylim)
  if (nr==1 && lty==1) {                # first time, plot everything..
    par(pty="s")
    plot(xdata, ydata, type=ptype, xaxt='n',yaxt='n', xlab=xlab, ylab=ylab,
         xlim=xlim, ylim=ylim, lwd=2, col=col, lty=lty, ...)
    ## draw the grid and axes
    l <- c(0.0001, 0.001, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 40, 60, 80, 90, 95, 98, 99, 99.5, 99.8, 99.9)
    qnl=qnorm(l/100)
    for (d in 1:2) axis(d,qnl,l)
    abline(h=qnl, lty=3)
    abline(v=qnl, lty=3)  
    ## y=x for equal error rate
    abline(coef=c(0,1), lty=3)
  }
  else
    lines(xdata, ydata, type=ptype, lty=lty, lwd=2, col=col)
  if (is.null(x)) return(NULL)
  summary(x)
}
