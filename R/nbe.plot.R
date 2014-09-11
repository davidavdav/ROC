## computes a normalized DCF plot for prior < 0.5
## d is of format det(sre)
`nbe.plot` <- function(x, main="Normalized Bayes Error rate", xlim=c(-10,5), ...) {
    r <- as.roc(x)
    x <- attr(r, "data")
    def.par <- par(no.readonly=T)          # save parameters...
    plo <- seq(xlim[1], xlim[2], by=0.1)
    norm.e <- pmin(sigmoid(plo), sigmoid(-plo))
    ## first the real LLR:
    pe <- bayes.error.rate(x$score[x$target], x$score[!x$target], plo) / norm.e
    plot(plo, pe, col=1, type="l", lwd=2, main=main,
         ylim=c(0,1), xlab = "logit effective prior", ylab="normalized bayes error", ...)
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
    fa <- round(r$pfa * attr(r, "stats")$nn)
    miss <- round(r$pmiss * attr(r, "stats")$nt)
    ## Doddington's rule of 30
    dr30i <- c(max(which(fa>=30)), min(which(miss>=30)))
    plo30 <- -r$opt.llr[dr30i]
#    cat("i ", dr30i, " plo ", plo30, "\n")
    norm.e <- pmin(sigmoid(plo30), sigmoid(-plo30))
    points(plo30, bayes.error.rate(x$opt.llr[x$target],
                                   x$opt.llr[!x$target], plo30) / norm.e, pch="*", cex=3)
}
