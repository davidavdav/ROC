## bless object with class "cst", collection of supervised trials
as.cst <- function(x) {
  if ("cst" %in% class(x)) return(x)
  if ("roc" %in% class(x)) return(attr(x, "data"))
  stopifnot("data.frame" %in% class(x), c("score", "target") %in% names(x), is.logical(x$target), is.numeric(x$score) || is.ordered(x$score))
  class(x) <- c("cst", class(x))
  return(x)
}
