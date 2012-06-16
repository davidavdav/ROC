## computes a normalized DCF plot for prior < 0.5
## d is of format det(sre)
`nbe.plot` <- function(d) {
  if ("sre" %in% class(d)) d <- det(d)
  else if (! "det" %in% class(d)) 
    stop(sprintf("The argument %s should be of class `det`", deparse(substitute(d))))
  def.par <- par(no.readonly=T)          # save parameters...
  plo <- seq(-10, 0, by=0.1)
  norm.e <- pmin(sigmoid(plo), sigmoid(-plo))
  ## first the real LLR:
  x <- d$data
  pe <- bayes.error.rate(x$score[x$target], x$score[!x$target], plo) / norm.e
  plot(plo, pe, col=1, type="l", lwd=2, main="Normalized Bayes Error rate",
       ylim=c(0,1), xlab = "logit effective prior", ylab="normalized bayes error")
  pe.miss <- bayes.miss.rate(x$score[x$target], plo) / norm.e
  pe.fa <- bayes.fa.rate(x$score[!x$target], plo) / norm.e
  lines(plo, pe.miss, col=2, lwd=1)
  lines(plo, pe.fa, col=3, lwd=1)
  ## then the optimal LLR:
  pe <- bayes.error.rate(x$opt.llr[x$target], x$opt.llr[!x$target], plo) / norm.e
  lines(plo, pe, col=1, lty=2, lwd=2)
  pe.miss <- bayes.miss.rate(x$opt.llr[x$target], plo) / norm.e
  pe.fa <- bayes.fa.rate(x$opt.llr[!x$target], plo) / norm.e
  lines(plo, pe.miss, col=2, lwd=1, lty=2)
  lines(plo, pe.fa, col=3, lwd=1, lty=2)
  ## NIST 2010 ref
  abline(v=c(log(1/999), log(1/9.9)), lty=2, col="magenta")
  legend("topright",
         legend=c("norm act DCF", "act miss rate", "act FA rate", "min DCF", "min miss", "min FA"),
         lty=rep(1:2, each=3), col=rep(1:3, times=2), lwd=rep(c(2,1,1),2))
  fa <- sum(1-x$target) - cumsum(1-x$target)
  dr30i <- max(which(fa>=30))
  plo30 <- -x$opt.llr[dr30i]
  points(plo30, bayes.error.rate(x$opt.llr[x$target],
                                 x$opt.llr[!x$target], plo30) / sigmoid(plo30), pch="*", cex=3)
}
