`plot.det` <-
function(x, nr=1, lty=1, col=nr, optimize=T,
         xmin=0.1, xmax=50, ymin=0.1, ymax=50,
         xlab="false alarm probability (%)",
         ylab="miss probability (%)",
         rocch=F, Ninterp=10,
         ...) {
  xlim <- c(nd(xmin), nd(xmax))         # 0.5 % seems accurately enough
  ylim <- c(nd(ymin), nd(ymax))
  par(pty="s", cex.axis=1)
  if (is.null(x))                      # only produce frame...
    xdata <- ydata <- numeric(0)        # empty data set
  else {
    attach(x)
    size <- length(fa)
    if (optimize) {
      changes <- diff(diff(fa)!=0)<0 | diff(diff(miss)!=0)<0
      sample <- c(T, changes, T)
    } else sample <- 1:size
    if (rocch) {
      ## keep it simple draw straight lines
      nseg <- length(ch)-1
      ninter <- nseg*Ninterp+1
      xdata <- ydata <- numeric(ninter)
      fa <- fa[ch]
      miss <- miss[ch]
      for (i in 1:nseg) {
        ## interpolate
        xx <- fa[i]+(0:9)*(fa[i+1]-fa[i])/Ninterp
        yy <- miss[i]+(0:9)*(miss[i+1]-miss[i])/Ninterp
        xdata[i*Ninterp+1:Ninterp] <- qnorm(xx)
        ydata[i*Ninterp+1:Ninterp] <- qnorm(yy)
      }
      xdata[ninter] <- qnorm(fa[nseg+1])
      ydata[ninter] <- qnorm(miss[nseg+1])
    } else {
      xdata <- qnorm(fa[sample])
      ydata <- qnorm(miss[sample])
    }
    detach(x)
  }
  if (nr==1 && lty==1) {                # first time, plot everything..
    plot(xdata, ydata, type="l", xaxt='n',yaxt='n', xlab=xlab, ylab=ylab,
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
    lines(xdata, ydata, type="l", lty=lty, lwd=2, col=col)
  ## actual DCF
  if (is.null(x)) return(NULL)
  rect(qnorm(x$afa.lci), qnorm(x$amiss.lci),
       qnorm(x$afa.uci), qnorm(x$amiss.uci), border=col, lwd=2)
  points(qnorm(x$mfa), qnorm(x$mmiss), pch=1, cex=2, col=col, lwd=2)
  summary(x)
}

