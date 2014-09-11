## function that tries to coerse its argument to class ROC

as.roc <- function(x) {
    cl <- class(x)
    if ("roc" %in% cl)
        return(x)
    else 
        return(roc(as.cst(x)))
}
