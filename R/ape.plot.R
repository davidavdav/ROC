`ape.plot` <-
function(data, legend="Ape plot", bar=T, bw=F) {
  def.par <- par(no.readonly=T)          # save parameters...
  if (class(data)=="det") data <- list(data)
  nr <- 1+as.numeric(bar)
  nsys <- length(data)
  layout(matrix(c(1,1:nsys,rep(nsys+1,nsys+1)),nr,nsys+1, byrow=T),
         c(1,rep(5,nsys)), c(3,2))
  legend <- rep(legend, nsys)
  clog <- clog.min <- vector(mode="numeric", length=nsys)
  lpo <- seq(-7, 7, by=0.1)
  pe <- pe.min <- pe.ref <- matrix(nrow=length(lpo), ncol=nsys)
  for (i in 1:nsys) {
    x <- data[[i]]$data
    x <- opt.llr(x, laplace=F) ## add optimal llr columns
    clog[i] <- Cllr(x)
    clog.min[i] <- Cllr(x, opt=T)
    pe[,i] <- bayes.error.rate(x$score[x$target], x$score[!x$target], lpo)
    pe.min[,i] <- bayes.error.rate(x$opt.llr[x$target],
                                   x$opt.llr[!x$target], lpo)
    pe.ref[,i] <- bayes.error.rate(0, 0, lpo);
  }
  max.e <- max(pe)
  par(cex=1)
  if (bw) {
    col <- rep(1,4);
    lty <- c(1,2,3,2)
  } else {
    col <- c("red", "green", "black", "purple")
    lty <- c(1,1,2,2)
  }
  for (i in 1:nsys) {
    if (i==1)
      par(mar=c(5,4,2,1))
    else
      par(mar=c(5,2,2,1))
    plot(lpo, pe[,i], col=col[1], type="l", lwd=2, main=legend[[i]],
         ylim=c(0,max.e), panel.first=grid(2, NULL, lty=lty[1]),
         xlab="log prior odds", ylab="probability of error")
    lines(lpo, pe.min[,i], col=col[2], lty=lty[2], lwd=2)
    lines(lpo, pe.ref[,i], col=col[3], lty=lty[3], lwd=2)
    abline(v=log(1/9.9), col=col[4], lty=lty[4])
  }
  if (bar) {
    bars <- rbind(clog.min, clog-clog.min)
    par(mar=c(3,4,0,0))
    if (bw) {
      col <- grey((2:1)/3)
    } else {
      col <- c("green", "red")
    }
    b <- barplot(bars, col=col, beside=F, ylab="Cllr (bits)")
    m <- max(clog)
    mx <- mean(b)
    legend(mx, 0.1*m, xjust=0.5, yjust=0,
           legend=c("calibration loss", "discrimination loss"),
           fill=rev(col), bg="white")
  }
  par(def.par)                          # reset parameters
}

