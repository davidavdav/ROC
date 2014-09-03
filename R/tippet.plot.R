tippet.plot <- function(x, ...) {
    x <- as.roc(x)
    LLR <- x$thres/log(10)
#    r <- rep(c(1,2,1), c(1,length(x$pfa)-2,1))
#    fa <- rep(x$pfa, r)
#    miss <- rep(x$pmiss, r)
    plot(LLR, x$pfa, xlab="10log LR larger than", ylab="fraction of trials", main="Tippet plot",
         type="l", ...)
    lines(LLR, 1-x$pmiss, ...)
    grid()
    r <- range(LLR, finite=T)
    text(r[1], 0, adj=c(0,0), labels="different source")
    text(r[2], 1, adj=c(1,1), labels="same source")
}

  
