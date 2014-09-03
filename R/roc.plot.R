## an explicit call to plot(roc)

roc.plot <- function(x, ...) {
    plot(as.roc(x), ...)
}
