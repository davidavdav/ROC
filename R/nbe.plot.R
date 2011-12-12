## computes a normalized DCF plot for prior < 0.5
## data is of format det(sre)
`nbe.plot` <- function(data) {
  if (! "det" %in% class(data)) 
    stop(sprintf("The argument %s should be of class `det`", deparse(substitute(data))))
  def.par <- par(no.readonly=T)          # save parameters...
  lpo <- seq(-10, 0, by=0.1)
  norm.e <- pmin(sigmoid(lpo), sigmoid(-lpo))
  ## first the real LLR:
  x <- data$data
  pe <- bayes.error.rate(x$score[x$target], x$score[!x$target], lpo) / norm.e
  plot(lpo, pe, col=1, type="l", lwd=2, main="Normalized Bayes Error rate", ylim=c(0,1), xlab = "logit effective prior", ylab="normalized bayes error")
  pe.miss <- bayes.miss.rate(x$score[x$target], lpo) / norm.e
  pe.fa <- bayes.fa.rate(x$score[!x$target], lpo) / norm.e
  lines(lpo, pe.miss, col=2, lwd=1)
  lines(lpo, pe.fa, col=3, lwd=1)
  ## then the optimal LLR:
  pe <- bayes.error.rate(x$opt.llr[x$target], x$opt.llr[!x$target], lpo) / norm.e
  lines(lpo, pe, col=1, lty=2, lwd=2)
  pe.miss <- bayes.miss.rate(x$opt.llr[x$target], lpo) / norm.e
  pe.fa <- bayes.fa.rate(x$opt.llr[!x$target], lpo) / norm.e
  lines(lpo, pe.miss, col=2, lwd=1, lty=2)
  lines(lpo, pe.fa, col=3, lwd=1, lty=2)
  ## NIST 2010 ref
  abline(v=c(log(1/999), log(1/9.9)), lty=2, col="magenta")
  legend("topright", legend=c("norm act DCF", "act miss rate", "act FA rate", "min DCF", "min miss", "min FA"), lty=rep(1:2, each=3), col=rep(1:3, times=2), lwd=rep(c(2,1,1),2))
  fa <- cumsum(x$target)
  dr30i <- which(fa==30)[1]
  lpo30 <- x$opt.llr[dr30i]
  points(lpo30, bayes.error.rate(x$opt.llr[x$target], x$opt.llr[!x$target], lpo30) / sigmoid(lpo30), pch="*", cex=3)
}
