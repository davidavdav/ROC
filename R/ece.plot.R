ece.plot <- function(x, main="ECE plot", ...) {
    x <- attr(as.roc(x), "data")        # add opt.llr column
    plo <- seq(-10, 10, by=0.1)
    plot(plo, ece.tnt(x$score[x$target], x$score[!x$target], plo=plo), type="l", main=main, xlab="prior log odds", ylab="empirical cross entropy (bits)", col="red", lwd=2, panel.first=grid(), ...)
    lines(plo, ece.tnt(x$opt.llr[x$target], x$opt.llr[!x$target], plo=plo), col="green", lwd=2)
    lines(plo, ece.tnt(0, 0, plo=plo), col="black", lty=2, lwd=2)
}
