## function that tries to coerse its argument to class ROC

as.roc <- function(x) {
    cl <- class(x)
    if ("roc" %in% cl)
        return(x)
    else if ("cst" %in% cl)
        return(roc(x))
    else if (is.data.frame(x) && all(is.numeric(x$score), is.logical(x$target)))

        return(roc(as.cst(x)))
    else
        stop("Argument must be of class cst, roc or a data.frame with numeric $score and logical $target")
}
