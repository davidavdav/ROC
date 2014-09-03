llr.plot <- function(x, ...) {
    x <- as.roc(x)
    ptype <- ifelse(nrow(x) > 15, "l", "b")
    with(x, plot(thres, opt.llr, xlab="score", ylab="LLR", main="Optimal score to LLR mapping",
                 type=ptype, ...))
}
